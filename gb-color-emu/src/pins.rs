use log::*;
use std::fmt;

use crate::cpu::ppu::LcdStatus;

#[derive(Copy, Clone)]
pub enum PinDriverSource<T: Copy> {
    NoSource,
    External { value: T },
    Internal { value: T },
}

impl<T: Copy> Default for PinDriverSource<T> {
    fn default() -> Self {
        Self::NoSource
    }
}

impl fmt::Debug for PinDriverSource<bool> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoSource => f.write_str("<unset>"),
            Self::External { value } => write!(f, "{:?} [inbound]", value),
            Self::Internal { value } => write!(f, "{:?} [outbound]", value),
        }
    }
}

impl fmt::Debug for PinDriverSource<u8> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoSource => f.write_str("<unset>"),
            Self::External { value } => write!(f, "{:#04x} [inbound]", value),
            Self::Internal { value } => write!(f, "{:#04x} [outbound]", value),
        }
    }
}

impl fmt::Debug for PinDriverSource<u16> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoSource => f.write_str("<unset>"),
            Self::External { value } => write!(f, "{:#06x} [inbound]", value),
            Self::Internal { value } => write!(f, "{:#06x} [outbound]", value),
        }
    }
}

impl fmt::Debug for PinDriverSource<LcdStatus> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoSource => f.write_str("<unset>"),
            Self::External { value } => write!(f, "{:?} [inbound]", value),
            Self::Internal { value } => write!(f, "{:?} [outbound]", value),
        }
    }
}

struct FieldRef<T: Copy> {
    label: &'static str,
    type_name: &'static str,
    value: PinDriverSource<T>,
}

impl<T: Copy> fmt::Display for FieldRef<T>
where
    PinDriverSource<T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}` on {} (with value {:?})", self.label, self.type_name, self.value)
    }
}

impl<T: Copy + std::cmp::Eq> PinDriverSource<T> {
    pub(crate) fn connect<T1, T2>(&mut self, other: &mut Self, label: &'static str)
    where
        PinDriverSource<T>: fmt::Debug,
    {
        let left = FieldRef { label, type_name: std::any::type_name::<T1>(), value: *self };
        let right = FieldRef { label, type_name: std::any::type_name::<T2>(), value: *other };
        match (*self, *other) {
            (Self::NoSource, Self::NoSource) => (),
            (Self::NoSource, Self::External { value }) => {
                trace!("Setting {} <- {}", left, right);
                *self = Self::External { value };
            }
            (Self::NoSource, Self::Internal { value }) => {
                trace!("Setting {} <- {}", left, right);
                *self = Self::External { value };
            }
            (Self::External { value }, Self::NoSource) => {
                trace!("Setting {} -> {}", left, right);
                *other = Self::External { value };
            }
            (Self::Internal { value }, Self::NoSource) => {
                trace!("Setting {} -> {}", left, right);
                *other = Self::External { value };
            }
            (Self::External { value: a }, Self::External { value: b }) => {
                if a != b {
                    trace!("Setting {} <- {}", left, right);
                    *self = Self::External { value: b };
                }
            }
            (Self::External { value: a }, Self::Internal { value: b }) => {
                if a != b {
                    trace!("Setting {} <- {}", left, right);
                    *self = Self::External { value: b };
                }
            }
            (Self::Internal { value: a }, Self::External { value: b }) => {
                if a != b {
                    trace!("Setting {} -> {}", left, right);
                    *other = Self::External { value: a };
                }
            }
            (Self::Internal { value: a }, Self::Internal { value: b }) if a != b => panic!(
                "Two components ({} and {}) are fighting over the {:?} {} wire",
                std::any::type_name::<T1>(),
                std::any::type_name::<T2>(),
                label,
                std::any::type_name::<T>(),
            ),
            (Self::Internal { .. }, Self::Internal { .. }) => (),
        }
    }
}
