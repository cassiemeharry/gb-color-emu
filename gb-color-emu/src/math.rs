use std::ops;

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub(crate) struct Matrix1D<const N: usize> {
    pub values: [f32; N],
}

impl<const X: usize> Matrix1D<X> {
    pub(crate) fn map(&self, f: impl FnMut(f32) -> f32) -> Self {
        let mut new = self.clone();
        for x in 0..X {
            new.values[x] = f(self.values[x]);
        }
        new
    }

    pub(crate) fn clamp(&self, min: f32, max: f32) -> Self {
        self.map(move |val| val.clamp(min, max))
    }

    pub(crate) fn powf(&self, power: f32) -> Self {
        self.map(move | val| val.powf(power))
    }
}

impl<const X: usize> From<[f32; X]> for Matrix1D<X> {
    fn from(values: [f32; X]) -> Self {
        Self { values }
    }
}

impl<const X: usize> From<f32> for Matrix1D<X> {
    fn from(value: f32) -> Self {
        Self { values: [value; X] }
    }
}

impl<'a, const X: usize> ops::Mul<f32> for &'a Matrix1D<X> {
    type Output = Matrix1D<X>;
    fn mul(self, rhs: f32) -> Matrix1D<X> {
        let mut output = self.clone();
        for x in 0..X {
            output.values[x] = self.values[x] * rhs;
        }
        output
    }
}

impl<const X: usize> ops::Mul<f32> for Matrix1D<X> {
    type Output = Matrix1D<X>;
    fn mul(self, rhs: f32) -> Matrix1D<X> {
        (&self).mul(rhs)
    }
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub(crate) struct Matrix2D<const X: usize, const Y: usize> {
    pub values: [[f32; X]; Y],
}

impl<const X: usize, const Y: usize> Matrix2D<X, Y> {
    pub(crate) fn map(&self, f: impl FnMut(f32) -> f32) -> Self {
        let mut new = self.clone();
        for y in 0..Y {
            for x in 0..X {
                new.values[y][x] = f(self.values[y][x]);
            }
        }
        new
    }

    pub(crate) fn clamp(&self, min: f32, max: f32) -> Self {
        self.map(move |val| val.clamp(min, max))
    }

    pub(crate) fn powf(&self, power: f32) -> Self {
        self.map(move | val| val.powf(power))
    }

    // pub(crate) fn powf_m1(&self, power: Matrix1D<X>) -> Self {
    //     let mut values = self.values.clone();
    //     for y in 0..Y {
    //         for x in 0..X {
    //             values[y][x] = self.values[y][x].powf(power[x]);
    //         }
    //     }
    //     Self { values }
    // }
}

impl<const X: usize> From<Matrix1D<X>> for Matrix2D<1, X> {
    fn from(vec: Matrix1D<X>) -> Self {
        let mut values = [[0.0]; X];
        for x in 0..X {
            values[x][0] = vec.values[x];
        }
        Matrix2D { values }
    }
}

impl<const X: usize, const Y: usize> From<[[f32; X]; Y]> for Matrix2D<X, Y> {
    fn from(values: [[f32; X]; Y]) -> Self {
        Self { values }
    }
}

impl<'a, const X: usize, const Y: usize> ops::Mul<&'a Matrix1D<Y>> for &'a Matrix2D<X, Y> {
    type Output = Matrix2D<Y, Y>;
    fn mul(self, rhs: &'a Matrix1D<Y>) -> Self::Output {
        let mut rhs = Matrix2D::<1, Y>::from(*rhs);
        self.mul(&rhs)
    }
}

impl<'a, const X: usize, const Y: usize> ops::Mul<f32> for &'a Matrix2D<X, Y> {
    type Output = Matrix2D<X, Y>;
    fn mul(self, rhs: f32) -> Matrix2D<X, Y> {
        let mut values = [[0.0; X]; Y];
        for y in 0..Y {
            for x in 0..X {
                values[y][x] *= rhs;
            }
        }
        Matrix2D { values }
    }
}

impl<'a, const X: usize, const Y: usize> ops::Mul<f32> for Matrix2D<X, Y> {
    type Output = Matrix2D<X, Y>;
    #[inline]
    fn mul(self, rhs: f32) -> Matrix2D<X, Y> {
        (&self).mul(rhs)
    }
}

impl<'a, const X: usize, const Y: usize, const Z: usize> ops::Mul<&'a Matrix2D<Y, Z>> for &'a Matrix2D<X, Y> {
    type Output = Matrix2D<Y, Z>;
    fn mul(self, rhs: &'a Matrix2D<Y, X>) -> Matrix2D<Y, Y> {
        let mut values = [[0.0; Y]; Y];
        for z in 0..Z {
            for y in 0..Y {
                let mut sum = 0.0;
                for x in 0..X {
                    sum += self.values[z][x] * rhs.values[x][y];
                }
                values[z][y] = sum;
            }
        }
        Matrix2D { values }
    }
}

impl<'a, const X: usize, const Y: usize> ops::Mul<Matrix2D<Y, X>> for &'a Matrix2D<X, Y> {
    type Output = Matrix2D<Y, Y>;
    #[inline(always)]
    fn mul(self, rhs: Matrix2D<Y, X>) -> Matrix2D<Y, Y> {
        self.mul(&rhs)
    }
}

impl<'a, const X: usize, const Y: usize> ops::Mul<&'a Matrix2D<Y, X>> for Matrix2D<X, Y> {
    type Output = Matrix2D<Y, Y>;
    #[inline(always)]
    fn mul(self, rhs: &'a Matrix2D<Y, X>) -> Matrix2D<Y, Y> {
        (&self).mul(rhs)
    }
}

impl<const X: usize, const Y: usize> ops::Mul<Matrix2D<Y, X>> for Matrix2D<X, Y> {
    type Output = Matrix2D<Y, Y>;
    #[inline(always)]
    fn mul(self, rhs: Matrix2D<Y, X>) -> Matrix2D<Y, Y> {
        (&self).mul(&rhs)
    }
}
