use std::{cmp::{Ord, Ordering}, fmt, mem, ops};

pub trait Key : Copy + fmt::Debug + Ord + ops::Sub<Self, Output = Self> {
    const MIN: Self;
    const MAX: Self;
}

impl Key for u16 {
    const MIN: u16 = u16::MIN;
    const MAX: u16 = u16::MAX;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Interval<K> {
    min: K,
    max: K,
}

impl<K: Key> Interval<K> {
    fn new(min: K, max: K) -> Self {
        debug_assert!(min <= max);
        Self { min, max }
    }

    pub fn union(self, other: Self) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    pub fn overlaps(self, other: Self) -> bool {
        !(self.min > other.max || self.max < other.min)
    }

    pub fn contains(self, point: K) -> bool {
        point >= self.min && point <= self.max
    }

    pub fn len(self) -> K {
        self.max - self.min
    }
}

#[derive(Clone)]
pub struct IntervalTree<K, V> {
    node: Node<K, V>,
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for IntervalTree<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.node.fmt(f)
    }
}

impl<K, V> IntervalTree<K, V> {
    pub fn new() -> Self {
        Self { node: Node::Empty }
    }
}

impl<K: Key, V> IntervalTree<K, V> {
    pub fn insert(&mut self, (lower, upper): (K, K), value: V) {
        debug_assert!(lower <= upper);
        let interval = Interval::new(lower.min(upper), lower.max(upper));
        debug!("Inserting interval {:04x?} into the tree", interval);
        self.node.insert(interval, value);
    }

    /// Gets the longest object that starts at a given key.
    pub fn get(&self, start: K) -> Option<&V> {
        let (_, value) = self.node.get(start)?;
        Some(value)
    }

    #[inline]
    pub fn remove_range(&mut self, range: impl ops::RangeBounds<K>) -> usize {
        let lower = match range.start_bound() {
            ops::Bound::Included(l) => *l,
            ops::Bound::Excluded(_) => todo!(),
            ops::Bound::Unbounded => K::MIN,
        };
        let upper = match range.end_bound() {
            ops::Bound::Included(u) => *u,
            ops::Bound::Excluded(_) => todo!(),
            ops::Bound::Unbounded => K::MIN,
        };
        let interval = Interval::new(lower.min(upper), lower.max(upper));
        self.node.remove_range(interval)
    }

    pub fn remove_one(&mut self, key: K) -> usize {
        let interval = Interval::new(key, key);
        self.node.remove_range(interval)
    }
}

#[derive(Clone)]
enum Node<K, V> {
    Empty,
    Leaf {
        bounds: Interval<K>,
        value: V,
    },
    Branch {
        bounds: Interval<K>,
        height: usize,
        left: Box<Node<K, V>>,
        right: Box<Node<K, V>>,
    }
}

impl<K, V> Default for Node<K, V> {
    fn default() -> Self {
        Self::Empty
    }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for Node<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bf = self.bf();
        match self {
            Self::Empty => f.debug_struct("Empty").finish(),
            Self::Leaf { bounds, value: _ } => f.debug_struct("Leaf")
                .field("bounds", bounds)
                .field("value", &format_args!("..."))
                .finish(),
            Self::Branch { bounds, height, left, right } => f.debug_struct("Branch")
                .field("bounds", bounds)
                .field("height", height)
                .field("BF", &bf)
                .field("left", left)
                .field("right", right)
                .finish(),
        }
    }
}

impl<K, V> Node<K, V> {
    fn height(&self) -> usize {
        match self {
            Self::Empty => 0,
            Self::Leaf { .. } => 1,
            Self::Branch { height, .. } => *height,
        }
    }

    fn bf(&self) -> isize {
        match self {
            Self::Empty => 0,
            Self::Leaf { .. } => 0,
            Self::Branch { left, right, .. } => right.height() as isize - left.height() as isize,
        }
    }
}

impl<K: Key, V> Node<K, V> {
    #[inline]
    fn bounds(&self) -> Interval<K> {
        match self {
            Self::Empty => Interval {
                // This is swapped on purpose, to represent an empty range.
                min: K::MAX,
                max: K::MIN,
            },
            Self::Leaf { bounds, .. } => *bounds,
            Self::Branch { bounds, .. } => *bounds,
        }
    }

    fn insert(&mut self, bounds: Interval<K>, value: V) {
        match self {
            Self::Empty => *self = Self::Leaf { bounds, value },
            Self::Leaf { .. } => {
                let prev = mem::take(self);
                let prev_bounds = prev.bounds();
                assert!(bounds != prev_bounds);
                let new = Self::Leaf { bounds, value };
                *self = Self::Branch {
                    bounds: prev.bounds().union(bounds),
                    height: prev.height().max(1) + 1,
                    left: Box::new(prev),
                    right: Box::new(new),
                };
            }
            Self::Branch { bounds: _, height: _, ref mut left, ref mut right} => {
                // We want to pick the branch where adding the new item would
                // increase the height the least, breaking ties by which would
                // increase the bounds the least.
                let new_left_size = left.bounds().union(bounds).len();
                let new_right_size = right.bounds().union(bounds).len();
                if new_left_size <= new_right_size {
                    left.insert(bounds, value);
                } else {
                    right.insert(bounds, value);
                }

                let bf = right.height() as isize - left.height() as isize;
                if bf >= 2 {
                    // Right side is too tall, rotate.
                    // X is self, Z is right
                    if right.bf() >= 0 {
                        self.rotate_left();
                    } else {
                        self.rotate_right_left();
                    }
                } else if bf <= -2 {
                    // Left side is too tall, rotate.
                    if left.bf() >= 0 {
                        self.rotate_left_right();
                    } else {
                        self.rotate_right();
                    }
                }
                match self {
                    Self::Empty | Self::Leaf { .. } => unreachable!(),
                    Self::Branch { bounds, height, left, right } => {
                        *bounds = left.bounds().union(right.bounds());
                        *height = left.height().max(right.height()) + 1;
                    }
                }
            }
        }
    }

    /// Gets the longest object that starts at a given key.
    fn get(&self, key: K) -> Option<(Interval<K>, &V)> {
        match self {
            Self::Empty => {
                // debug!("Tried to get with key {:#x?} from empty node", key);
                None
            },
            Self::Leaf { bounds, .. } if bounds.min != key => {
                // debug!("Tried to get with key {:#x?} from leaf that starts with {:#x?}", key, bounds.min);
                None
            }
            Self::Leaf { bounds, value } => Some((*bounds, value)),
            Self::Branch { bounds, .. } if !bounds.contains(key) => {
                // debug!("Tried to get with key {:#x?} from branch that doesn't contain it: bounds = {:#x?}", key, bounds);
                None
            }
            Self::Branch { bounds: _, height: _, left, right } => match (left.get(key), right.get(key)) {
                (None, None) => None,
                (Some(x), None) => Some(x),
                (None, Some(x)) => Some(x),
                (Some((lb, lv)), Some((rb, rv))) => match lb.len().cmp(&rb.len()) {
                    Ordering::Less => Some((lb, lv)),
                    Ordering::Greater => Some((rb, rv)),
                    Ordering::Equal => panic!("Found a duplicate value in the tree!"),
                }
            }
        }
    }

    /// Removes all `Leaf` nodes that overlap this range. Returns the number of
    /// nodes removed.
    fn remove_range(&mut self, bounds: Interval<K>) -> usize {
        match self {
            Self::Empty => 0,
            Self::Leaf { bounds: l_bounds, .. } if !l_bounds.overlaps(bounds) => 0,
            Self::Leaf { bounds: l_bounds, .. } => {
                debug!("Removing item from interval tree with bounds {:#06x?} (overlaps with requested bounds {:#06x?})", l_bounds, bounds);
                *self = Self::Empty;
                1
            }
            Self::Branch { bounds: b_bounds, .. } if !b_bounds.overlaps(bounds) => 0,
            Self::Branch { bounds: b_bounds, height, left, right } => {
                let l = left.remove_range(bounds);
                let r = right.remove_range(bounds);
                match (&**left, &**right) {
                    (Self::Empty, Self::Empty) => *self = Self::Empty,
                    (Self::Empty, _) => *self = mem::take(right),
                    (_, Self::Empty) => *self = mem::take(left),
                    _ => {
                        *b_bounds = left.bounds().union(right.bounds());
                        *height = left.height().max(right.height()) + 1;
                    }
                };
                l + r
            }
        }
    }

    /// Rotates a tree from this:
    ///
    /// ```text
    ///   self
    ///  /    \
    /// t1     Z
    ///       / \
    ///     t23  t4
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    ///     self
    ///    /    \
    ///   Z     t4
    ///  / \
    /// t1 t23
    /// ```
    fn rotate_left(&mut self) {
        assert!(self.bf() >= 2);
        let (left, mut right): (Box<Self>, Box<Self>) = match mem::take(self) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        assert!(right.bf() == 0 || right.bf() == 1);
        let (r_left, r_right): (Box<Self>, Box<Self>) = match mem::take(&mut *right) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let left_bounds = left.bounds().union(r_left.bounds());
        let left_height = left.height().max(r_left.height()) + 1;
        *right = Self::Branch {
            bounds: left_bounds,
            height: left_height,
            left,
            right: r_left,
        };
        *self = Self::Branch {
            bounds: left_bounds.union(r_right.bounds()),
            height: left_height.max(r_right.height()) + 1,
            left: right,
            right: r_right,
        };
    }

    /// Rotates a tree from this:
    ///
    /// ```text
    ///     self
    ///    /    \
    ///   Z     t4
    ///  / \
    /// t1 t23
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    ///   self
    ///  /    \
    /// t1     Z
    ///       / \
    ///     t23  t4
    /// ```
    fn rotate_right(&mut self) {
        assert!(self.bf() <= -2);
        let (mut z, t4): (Box<Self>, Box<Self>) = match mem::take(self) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let (t1, t23): (Box<Self>, Box<Self>) = match mem::take(&mut *z) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let z_bounds = t23.bounds().union(t4.bounds());
        let z_height = t23.height().max(t4.height()) + 1;
        *z = Self::Branch {
            bounds: z_bounds,
            height: z_height,
            left: t23,
            right: t4,
        };
        *self = Self::Branch {
            bounds: t1.bounds().union(z_bounds),
            height: t1.height().max(z_height) + 1,
            left: t1,
            right: z,
        };
    }

    /// Rotates a tree from this:
    ///
    /// ```text
    ///   self
    ///  /    \
    /// t1     Z
    ///       / \
    ///      Y  t4
    ///     / \
    ///    t2 t3
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    ///     self
    ///    /    \
    ///   Y      Z
    ///  / \    / \
    /// t1 t2  t3 t4
    /// ```
    fn rotate_right_left(&mut self) {
        let (t1, mut z): (Box<Self>, Box<Self>) = match mem::take(self) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_right_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let (mut y, t4): (Box<Self>, Box<Self>) = match mem::take(&mut *z) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_right_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let (t2, t3): (Box<Self>, Box<Self>) = match mem::take(&mut *y) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_right_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let y_bounds = t1.bounds().union(t2.bounds());
        let y_height = t1.height().max(t2.height()) + 1;
        *y = Self::Branch {
            bounds: y_bounds,
            height: y_height,
            left: t1,
            right: t2,
        };
        let z_bounds = t3.bounds().union(t4.bounds());
        let z_height = t3.height().max(t4.height()) + 1;
        *z = Self::Branch {
            bounds: z_bounds,
            height: z_height,
            left: t3,
            right: t4,
        };
        *self = Self::Branch {
            bounds: y_bounds.union(z_bounds),
            height: y_height.max(z_height) + 1,
            left: y,
            right: z,
        };
    }

    /// Rotates a tree from this:
    ///
    /// ```text
    ///     self
    ///    /    \
    ///   Z     t4
    ///  / \
    /// t1  Y
    ///    / \
    ///   t2 t3
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    ///     self
    ///    /    \
    ///   Y      Z
    ///  / \    / \
    /// t1 t2  t3 t4
    /// ```
    fn rotate_left_right(&mut self) {
        let (mut z, t4): (Box<Self>, Box<Self>) = match mem::take(self) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_right_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let (t1, mut y): (Box<Self>, Box<Self>) = match mem::take(&mut *z) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_right_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let (t2, t3): (Box<Self>, Box<Self>) = match mem::take(&mut *y) {
            Self::Empty | Self::Leaf { .. } => panic!("rotate_right_left on non-branch"),
            Self::Branch { left, right, .. } => (left, right),
        };
        let y_bounds = t1.bounds().union(t2.bounds());
        let y_height = t1.height().max(t2.height()) + 1;
        *y = Self::Branch {
            bounds: y_bounds,
            height: y_height,
            left: t1,
            right: t2,
        };
        let z_bounds = t3.bounds().union(t4.bounds());
        let z_height = t3.height().max(t4.height()) + 1;
        *z = Self::Branch {
            bounds: z_bounds,
            height: z_height,
            left: t3,
            right: t4,
        };
        *self = Self::Branch {
            bounds: y_bounds.union(z_bounds),
            height: y_height.max(z_height) + 1,
            left: y,
            right: z,
        };
    }
}

// macro_rules! simple_rotation {
//     (long = $long:ident, short = $short:ident) => {
//         fn [<rotate_ $short>]<K: Key, V>(top: &mut Node<K, V>) {
//         }
//     }
// }
