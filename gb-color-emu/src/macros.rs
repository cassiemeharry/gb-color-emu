macro_rules! pinout {
    ($(#[$meta:meta])* $struct_vis:vis struct $struct_name:ident {
        $( $inout:ident $field:ident : $ty:ty ,)*
    }) => {
        paste::paste! {
            // $(#[$meta])*
            // #[derive(Copy, Clone, Default)]
            // struct [< $struct_name Drivers>] {
            //     $(
            //         $field: ::core::option::Option<crate::pins::PinDriverSource>,
            //     )*
            // }

            $(#[$meta])*
            #[derive(Copy, Clone, Debug, Default)]
            $struct_vis struct $struct_name {
                $(pub(crate) $field : crate::pins::PinDriverSource<$ty>,)*
            }

            // impl ::core::fmt::Debug for $struct_name {
            //     fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            //         f.debug_struct(stringify!($struct_name))
            //             $(
            //                 .field(stringify!($field), &self.$field)
            //             )*
            //             .finish()
            //     }
            // }

            impl $struct_name {
                $(
                    pinout!(@access $inout $field : $ty);
                )*
            }
        }
    };
    (@access in $field:ident : $ty:ty) => {
        pinout!(@access $field : $ty ; pub getter, pub setter);
    };
    (@access out $field:ident : $ty:ty) => {
        pinout!(@access $field : $ty ; pub getter, setter);
    };
    (@access inout $field:ident : $ty:ty) => {
        pinout!(@access $field : $ty ; pub getter, pub setter);
    };
    (@access unk $field:ident : $ty:ty) => {
        // pinout!(@access $field : $ty ; getter, setter);
    };
    (@access $field:ident : $ty:ty ; $getter_vis:vis getter, $setter_vis:vis setter) => {
        paste::paste! {
            #[allow(non_snake_case, unused)]
            #[inline]
            fn [< get_ $field >](&self) -> $ty {
                use crate::pins::PinDriverSource as PDS;
                match self.$field {
                    PDS::NoSource => panic!(
                        "Tried to read an undriven field `{}` on {}",
                        stringify!($field),
                        ::core::any::type_name::<Self>(),
                    ),
                    PDS::Internal { value } => value,
                    PDS::External { value } => value,
                }
            }

            #[allow(non_snake_case, unused)]
            #[inline]
            $getter_vis fn [< get_ $field __external >](&self) -> $ty {
                use crate::pins::PinDriverSource as PDS;
                match self.$field {
                    PDS::NoSource => panic!(
                        "Tried to read an undriven field `{}` on {}",
                        stringify!($field),
                        ::core::any::type_name::<Self>(),
                    ),
                    PDS::Internal { value } => value,
                    PDS::External { value } => value,
                }
            }

            #[allow(non_snake_case, unused)]
            #[inline]
            $getter_vis fn [< get_ $field __opt >](&self) -> Option<$ty> {
                use crate::pins::PinDriverSource as PDS;
                match self.$field {
                    PDS::NoSource => None,
                    PDS::Internal { value } => Some(value),
                    PDS::External { value } => Some(value),
                }
            }

            #[allow(non_snake_case, unused)]
            #[inline(always)]
            $setter_vis fn [< set_ $field __external >](&mut self, value: $ty) {
                use crate::pins::PinDriverSource as PDS;
                self.$field = PDS::External { value };
            }

            #[allow(non_snake_case, unused)]
            #[inline(always)]
            $setter_vis fn [< set_ $field __external_opt >](&mut self, value: ::core::option::Option<$ty>) {
                use crate::pins::PinDriverSource as PDS;
                match value {
                    Some(value) => self.[< set_ $field __external>](value),
                    None => self.$field = PDS::NoSource,
                }
            }

            #[allow(non_snake_case, unused)]
            #[inline(always)]
            fn [< set_ $field >](&mut self, value: $ty) {
                use crate::pins::PinDriverSource as PDS;
                self.$field = PDS::Internal { value };
            }

            #[allow(non_snake_case, unused)]
            #[inline(always)]
            fn [< set_ $field __opt >](&mut self, value: ::core::option::Option<$ty>) {
                use crate::pins::PinDriverSource as PDS;
                match value {
                    Some(value) => self.[< set_ $field >](value),
                    None => self.$field = PDS::NoSource,
                }
            }

            #[allow(non_snake_case, unused)]
            #[inline(always)]
            fn [< release_ $field >](&mut self) {
                use crate::pins::PinDriverSource as PDS;
                match self.$field {
                    PDS::NoSource => (),
                    PDS::External { .. } => (),
                    PDS::Internal { .. } => self.$field = PDS::NoSource,
                }
            }
        }
    };
    (@debug_field $value:expr ; bool) => {
        $value
    };
    (@debug_field $value:expr ; u8) => {
        format_args!("{:#04x}", $value)
    };
    (@debug_field $value:expr ; u16) => {
        format_args!("{:#06x}", $value)
    };
    (@debug_field $value:expr ; $ty:ty) => {
        $value
    };
}

macro_rules! connect_pins {
    ($a:expr ; $aty:ty ; $b:expr ; $bty:ty ; [$($field:tt ,)+] $(,)*) => {
        $(
            (connect_pins!(@access left $a ; $field)).connect::<$aty, $bty>(
                &mut connect_pins!(@access right $b ; $field),
                connect_pins!(@label $field),
            );
        )+
    };
    (@access $_:ident $x:expr ; $field_name:ident) => {
        $x.$field_name
    };
    (@access left $x:expr ; [ $field_name:ident => $_:ident ]) => {
        $x.$field_name
    };
    (@access right $x:expr ; [ $_:ident => $field_name:ident ]) => {
        $x.$field_name
    };
    (@label $field_name:ident) => {
        stringify!($field_name)
    };
    (@label [ $field_name:ident => $_:ident ]) => {
        stringify!($field_name)
    };
}
