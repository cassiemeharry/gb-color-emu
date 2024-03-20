#![allow(non_camel_case_types)]
#![deny(unused)]
// #![feature(proc_macro_diagnostic)]

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream, Result},
    parse_macro_input,
    punctuated::Punctuated,
    token::Paren,
    Error, Ident, LitInt, Token,
};

fn single_byte(span: Span, val: u8) -> Result<TokenStream> {
    let val = LitInt::new_byte(val, span);
    Ok(quote_spanned!(span=> #val))
}

#[derive(Clone, Debug)]
struct Word {
    high: u8,
    low: u8,
    span: Span,
}

impl Parse for Word {
    fn parse(input: ParseStream) -> Result<Self> {
        let lit = input.parse::<LitInt>()?;
        let value = lit.base10_parse::<u16>()?;
        let high = (value >> 8) as u8;
        let low = value as u8;
        Ok(Self { high, low, span: lit.span() })
    }
}

impl quote::ToTokens for Word {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let l = LitInt::new_byte(self.low, self.span.clone());
        let h = LitInt::new_byte(self.high, self.span.clone());
        let new = quote::quote! { #l , #h };
        tokens.append_all(new);
    }
}

#[derive(Clone, Debug)]
struct UByte {
    value: u8,
    span: Span,
}

impl Parse for UByte {
    fn parse(input: ParseStream) -> Result<Self> {
        let lit = input.parse::<LitInt>()?;
        let value = lit.base10_parse::<u8>()?;
        Ok(Self { value, span: lit.span() })
    }
}

impl quote::ToTokens for UByte {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let val = LitInt::new_byte(self.value, self.span.clone());
        let new = quote_spanned!(self.span=> #val);
        tokens.append_all(new);
    }
}

#[derive(Clone, Debug)]
struct SByte {
    value: i8,
    span: Span,
}

impl SByte {
    fn invert(self) -> Self {
        Self {
            value: 0 - self.value,
            span: self.span,
        }
    }
}

impl Parse for SByte {
    fn parse(input: ParseStream) -> Result<Self> {
        let lit = input.parse::<LitInt>()?;
        let value = lit.base10_parse::<i8>()?;
        let span = lit.span();
        Ok(Self { value, span })
    }
}

impl quote::ToTokens for SByte {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let new_val = self.value as u8;
        let new_lit = LitInt::new(&format!("{:#04x}", new_val), self.span);
        let stream = quote::quote_spanned!(self.span=> #new_lit);
        tokens.append_all(stream);
    }
}

#[derive(Copy, Clone, Debug)]
enum R {
    B, C, D, E, H, L, IndHL, A,
}

impl Parse for R {
    fn parse(input: ParseStream) -> Result<Self> {
        const ERROR_MSG: &str = "Invalid byte register name, expected one of A, B, C, D, E, H, L, or (HL)";

        if input.peek(Ident) {
            let name = input.parse::<Ident>()?;
            let name_str = name.to_string().to_lowercase();
            match name_str.as_str() {
                "b" => Ok(Self::B),
                "c" => Ok(Self::C),
                "d" => Ok(Self::D),
                "e" => Ok(Self::E),
                "h" => Ok(Self::H),
                "l" => Ok(Self::L),
                "a" => Ok(Self::A),
                _ => Err(Error::new(name.span(), ERROR_MSG)),
            }
        } else if input.peek(Paren) {
            let content;
            let paren: Paren = parenthesized!(content in input);
            let name = content.parse::<Ident>()?;
            let name_str = name.to_string().to_lowercase();
            if &name_str == "hl" {
                Ok(Self::IndHL)
            } else {
                Err(Error::new(paren.span, ERROR_MSG))
            }
        } else {
            Err(Error::new(input.span(), ERROR_MSG))
        }
    }
}

impl R {
    fn offset_row(self) -> u8 {
        match self {
            Self::B => 0,
            Self::C => 1,
            Self::D => 2,
            Self::E => 3,
            Self::H => 4,
            Self::L => 5,
            Self::IndHL => 6,
            Self::A => 7,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum RP {
    BC, DE, HL, SP,
}

impl Parse for RP {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;
        let name_str = name.to_string().to_lowercase();
        match name_str.as_str() {
            "bc" => Ok(Self::BC),
            "de" => Ok(Self::DE),
            "hl" => Ok(Self::HL),
            "sp" => Ok(Self::SP),
            _ => Err(Error::new(name.span(), "Invalid wide register name, expected one of BC, DE, HL, or SP")),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum IndHLIndex {
    HLI, HLD
}

impl Parse for IndHLIndex {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        parenthesized!(content in input);
        let name = content.parse::<Ident>()?;
        let name_str = name.to_string().to_lowercase();
        match name_str.as_str() {
            "hli" => Ok(Self::HLI),
            "hld" => Ok(Self::HLD),
            _ => Err(Error::new(name.span(), "Invalid index register action, expected one of HLI or HLD")),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum RP2 {
    BC, DE, HL, AF,
}

impl Parse for RP2 {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;
        let name_str = name.to_string().to_lowercase();
        match name_str.as_str() {
            "bc" => Ok(Self::BC),
            "de" => Ok(Self::DE),
            "hl" => Ok(Self::HL),
            "af" => Ok(Self::AF),
            _ => Err(Error::new(name.span(), "Invalid wide register name, expected one of BC, DE, HL, or AF")),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum CC {
    NZ, Z, NC, C,
}

impl Parse for CC {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;
        let name_str = name.to_string().to_lowercase();
        match name_str.as_str() {
            "nz" => Ok(Self::NZ),
            "z" => Ok(Self::Z),
            "nc" => Ok(Self::NC),
            "c" => Ok(Self::C),
            _ => Err(Error::new(name.span(), "Invalid condition code, expected one of NZ, Z, NC, or C")),
        }
    }
}

// #[derive(Copy, Clone, Debug)]
// enum ALU {
//     AddA, AdcA, Sub, SbcA, And, Xor, Or, Cp,
// }

// #[derive(Copy, Clone, Debug)]
// enum ROT {
//     RLC, RRC, RL, RR, SLA, SRA, SLL, SRL,
// }

#[derive(Clone, Debug)]
enum Add {
    A_Lit1(UByte),
    A_R(R),
    HL_RP(RP),
    SP_Lit1(SByte),
}

impl Parse for Add {
    fn parse(input: ParseStream) -> Result<Self> {
        const LHS_ERROR: &str = "Invalid `ADD` instruction. Left hand side must be either `A`, `HL`, or `SP`";

        let span = input.span();
        if input.fork().parse::<RP>().is_ok() {
            match input.parse()? {
                RP::HL => {
                    let _ = input.parse::<Token![,]>()?;
                    let rp = input.parse()?;
                    Ok(Self::HL_RP(rp))
                }
                RP::SP => {
                    let _ = input.parse::<Token![,]>()?;
                    let offset = input.parse()?;
                    Ok(Self::SP_Lit1(offset))
                }
                _ => Err(Error::new(span, LHS_ERROR)),
            }
        } else {
            match input.parse()? {
                R::A => (),
                _ => return Err(Error::new(span, LHS_ERROR)),
            };
            let _ = input.parse::<Token![,]>()?;
            if input.fork().parse::<R>().is_ok() {
                let r = input.parse()?;
                Ok(Self::A_R(r))
            } else {
                let byte = input.parse()?;
                Ok(Self::A_Lit1(byte))
            }
        }
    }
}

impl Add {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self {
            Self::A_Lit1(lit) => Ok(quote_spanned!(span=> 0xc6, #lit )),
            Self::A_R(r) => single_byte(span, 0x80 + r.offset_row()),
            Self::HL_RP(pair) => {
                let opcode_val = match pair {
                    RP::BC => 0x09,
                    RP::DE => 0x19,
                    RP::HL => 0x29,
                    RP::SP => 0x39,
                };
                single_byte(span, opcode_val)
            }
            Self::SP_Lit1(l) => Ok(quote_spanned!(span=> 0xe8, #l )),
        }
    }
}

#[derive(Clone, Debug)]
enum RegOrLit1 {
    Lit1(UByte),
    R(R),
}

impl Parse for RegOrLit1 {
    fn parse(input: ParseStream) -> Result<Self> {
        const LHS_ERROR: &str = "Invalid instruction. Left hand side must be A";

        let span = input.span();
        match input.parse()? {
            R::A => (),
            _ => return Err(Error::new(span, LHS_ERROR)),
        };
        let _ = input.parse::<Token![,]>()?;
        if input.fork().parse::<R>().is_ok() {
            let r = input.parse()?;
            Ok(Self::R(r))
        } else {
            let byte = input.parse()?;
            Ok(Self::Lit1(byte))
        }
    }
}

struct ACommaReg {
    reg: R,
}

impl Parse for ACommaReg {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        match input.parse()? {
            R::A => (),
            _ => return Err(Error::new(span, "Expected first argument to be A register")),
        };
        let _ = input.parse::<Token![,]>()?;
        let reg = input.parse()?;
        Ok(Self { reg })
    }
}

impl From<ACommaReg> for R {
    #[inline(always)]
    fn from(ACommaReg { reg }: ACommaReg) -> R {
        reg
    }
}

#[derive(Clone, Debug)]
struct BitCommaReg {
    bit: u8,
    reg: R,
}

impl Parse for BitCommaReg {
    fn parse(input: ParseStream) -> Result<Self> {
        let byte = input.parse::<UByte>()?;
        let bit = byte.value;
        if bit > 7 {
            return Err(Error::new(byte.span, "Bit number must be in range 0..=7"));
        }
        let _ = input.parse::<Token![,]>()?;
        let reg = input.parse()?;
        Ok(Self { bit, reg })
    }
}

impl BitCommaReg {
    fn to_lit_tokens(self, span: Span, base: u8) -> Result<TokenStream> {
        let opcode = base + (self.bit << 3) + self.reg.offset_row();
        let l = LitInt::new_byte(opcode, span);
        Ok(quote_spanned!(span=> 0xcb, #l))
    }
}

#[derive(Clone, Debug)]
struct Call {
    cc: Option<CC>,
    target: Word,
}

impl Parse for Call {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.fork().parse::<Word>().is_ok() {
            let target = input.parse()?;
            Ok(Self {
                cc: None,
                target,
            })
        } else {
            let cc = Some(input.parse()?);
            let _: Token![,] = input.parse()?;
            let target = input.parse()?;
            Ok(Self { cc, target })
        }
    }
}

impl Call {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        let opcode_val = match self.cc {
            None => 0xcd,
            Some(CC::NZ) => 0xc4,
            Some(CC::Z) => 0xcc,
            Some(CC::NC) => 0xd4,
            Some(CC::C) => 0xdc,
        };
        let opcode = LitInt::new_byte(opcode_val, span);
        let target = self.target;
        Ok(quote_spanned!(span=> #opcode, #target))
    }
}

#[derive(Clone, Debug)]
enum Dec {
    R(R),
    RP(RP),
}

impl Parse for Dec {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.fork().parse::<RP>().is_ok() {
            let rp = input.parse()?;
            Ok(Self::RP(rp))
        } else if input.fork().parse::<R>().is_ok() {
            let r = input.parse()?;
            Ok(Self::R(r))
        } else {
            Err(Error::new(input.span(), "Invalid DEC instruction, expected a register name here"))
        }
    }
}

impl Dec {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self {
            Self::R(r) => {
                let opcode_val = match r {
                    R::B => 0x05,
                    R::C => 0x0d,
                    R::D => 0x15,
                    R::E => 0x1d,
                    R::H => 0x25,
                    R::L => 0x2d,
                    R::IndHL => 0x35,
                    R::A => 0x3d,
                };
                single_byte(span, opcode_val)
            }
            Self::RP(pair) => {
                let opcode_val = match pair {
                    RP::BC => 0x0b,
                    RP::DE => 0x1b,
                    RP::HL => 0x2b,
                    RP::SP => 0x3b,
                };
                single_byte(span, opcode_val)
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Inc {
    R(R),
    RP(RP),
}

impl Parse for Inc {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.fork().parse::<RP>().is_ok() {
            let rp = input.parse()?;
            Ok(Self::RP(rp))
        } else if input.fork().parse::<R>().is_ok() {
            let r = input.parse()?;
            Ok(Self::R(r))
        } else {
            Err(Error::new(input.span(), "Invalid INC instruction, expected a register name here"))
        }
    }
}

impl Inc {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self {
            Self::R(r) => {
                let opcode_val = match r {
                    R::B => 0x04,
                    R::C => 0x0c,
                    R::D => 0x14,
                    R::E => 0x1c,
                    R::H => 0x24,
                    R::L => 0x2c,
                    R::IndHL => 0x34,
                    R::A => 0x3c,
                };
                single_byte(span, opcode_val)
            }
            Self::RP(pair) => {
                let opcode_val = match pair {
                    RP::BC => 0x03,
                    RP::DE => 0x13,
                    RP::HL => 0x23,
                    RP::SP => 0x33,
                };
                single_byte(span, opcode_val)
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Jp {
    CC_Lit2(CC, Word),
    HL,
    Lit2(Word),
}

impl Parse for Jp {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        if input.fork().parse::<Word>().is_ok() {
            let target = input.parse()?;
            Ok(Self::Lit2(target))
        } else if let Some(rp) = input.try_parenthesized::<RP>() {
            // span.unwrap().warning().emit();
            println!("`JP (HL)` is misleading, as the address jumped to is the value in `HL`, not the value in memory pointed at by `HL`.");
            // span.unwrap().note().emit();
            println!("Consider using `JP HL` instead.");
            match rp {
                // TODO: show a warning about how this notation is misleading, and should be `JP HL` instead.
                RP::HL => Ok(Self::HL),
                _ => Err(Error::new(span, "Invalid `JP` target. Indirecting through registers only supports `HL`")),
            }
        } else if input.fork().parse::<RP>().is_ok() {
            match input.parse()? {
                RP::HL => Ok(Self::HL),
                _ => Err(Error::new(span, "Invalid `JP` target. Indirecting through registers only supports `HL`")),
            }
        } else {
            let cc = input.parse()?;
            let _: Token![,] = input.parse()?;
            let target = input.parse()?;
            Ok(Self::CC_Lit2(cc, target))
        }
    }
}

impl Jp {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        let (opcode_val, target) = match self {
            Self::CC_Lit2(CC::NZ, l) => (0xc2, Some(l)),
            Self::CC_Lit2(CC::Z, l) => (0xca, Some(l)),
            Self::CC_Lit2(CC::NC, l) => (0xd2, Some(l)),
            Self::CC_Lit2(CC::C, l) => (0xda, Some(l)),
            Self::HL => (0xe9, None),
            Self::Lit2(l) => (0xc3, Some(l)),
        };
        let opcode = LitInt::new_byte(opcode_val, span);
        match target {
            Some(t) => Ok(quote_spanned!(span=> #opcode, #t )),
            None => Ok(quote_spanned!(span=> #opcode )),
        }
    }
}

#[derive(Clone, Debug)]
struct Jr {
    cc: Option<CC>,
    offset: SByte,
}

impl Parse for Jr {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.fork().parse::<SByte>().is_ok() {
            let offset = input.parse()?;
            Ok(Self {
                cc: None,
                offset,
            })
        } else {
            let cc = Some(input.parse()?);
            let _: Token![,] = input.parse()?;
            let offset = input.parse()?;
            Ok(Self { cc, offset })
        }
    }
}

impl Jr {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        let opcode_val = match self.cc {
            None => 0x18,
            Some(CC::NZ) => 0x20,
            Some(CC::Z) => 0x28,
            Some(CC::NC) => 0x30,
            Some(CC::C) => 0x38,
        };
        let opcode = LitInt::new_byte(opcode_val, span);
        let offset = self.offset;
        Ok(quote_spanned!(span=> #opcode, #offset))
    }
}

#[derive(Clone, Debug)]
struct Ret {
    cc: Option<CC>,
}

impl Parse for Ret {
    fn parse(input: ParseStream) -> Result<Self> {
        let cc = input.parse().ok();
        Ok(Self { cc })
    }
}

impl Ret {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self.cc {
            None => single_byte(span, 0xc9),
            Some(cc) => single_byte(span, 0xc0 + 8 * (cc as u8)),
        }
    }
}

#[derive(Clone, Debug)]
enum Rst {
    Rst00,
    Rst08,
    Rst10,
    Rst18,
    Rst20,
    Rst28,
    Rst30,
    Rst38,
}

impl Parse for Rst {
    fn parse(input: ParseStream) -> Result<Self> {
        let offset = input.parse::<LitInt>()?;
        match offset.base10_parse::<u8>()? {
            0x00 => Ok(Self::Rst00),
            0x08 => Ok(Self::Rst08),
            0x10 => Ok(Self::Rst10),
            0x18 => Ok(Self::Rst18),
            0x20 => Ok(Self::Rst20),
            0x28 => Ok(Self::Rst28),
            0x30 => Ok(Self::Rst30),
            0x38 => Ok(Self::Rst38),
            _ => Err(Error::new(offset.span(), "RST vector must be one of 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, or 0x38")),
        }
    }
}

impl Rst {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self {
            Self::Rst00 => single_byte(span, 0xc7),
            Self::Rst08 => single_byte(span, 0xcf),
            Self::Rst10 => single_byte(span, 0xd7),
            Self::Rst18 => single_byte(span, 0xdf),
            Self::Rst20 => single_byte(span, 0xe7),
            Self::Rst28 => single_byte(span, 0xef),
            Self::Rst30 => single_byte(span, 0xf7),
            Self::Rst38 => single_byte(span, 0xff),
        }
    }
}

#[derive(Clone, Debug)]
enum Ld {
    A_IndBC,
    A_IndDE,
    A_IndHLIndex(IndHLIndex),
    A_IndLit2(Word),
    HL_SP_Offset(SByte),
    IndBC_A,
    IndDE_A,
    IndHLIndex_A(IndHLIndex),
    IndLit2_A(Word),
    IndLit2_SP(Word),
    RP_Lit2(RP, Word),
    R_Lit1(R, UByte),
    R_R(R, R),
    SP_HL,
}

impl Parse for Ld {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        if input.fork().parse::<R>().is_ok() {
            let r_span = input.span();
            let r = input.parse()?;
            let _: Token![,] = input.parse()?;
            if input.fork().parse::<LitInt>().is_ok() {
                let lit = input.parse()?;
                Ok(Self::R_Lit1(r, lit))
            } else if input.fork().parse::<R>().is_ok() {
                let r2: R = input.parse()?;
                match (r, r2) {
                    (R::IndHL, R::IndHL) => Err(Error::new(span, "LD (HL), (HL) is invalid (same encoding as the HALT instruction)")),
                    _ => Ok(Self::R_R(r, r2)),
                }
            } else if let Some(rp) = input.try_parenthesized::<RP>() {
                match (r, rp) {
                    (R::A, RP::BC) => Ok(Self::A_IndBC),
                    (R::A, RP::DE) => Ok(Self::A_IndDE),
                    (R::A, _) => Err(Error::new(r_span, "Invalid LD instruction, indirect reads from register pair can only come from BC and DE")),
                    _ => Err(Error::new(r_span, "Invalid LD instruction, indirect reads from a register pair must go to A")),
                }
            } else if let Some(lit) = input.try_parenthesized::<Word>() {
                match r {
                    R::A => Ok(Self::A_IndLit2(lit)),
                    _ => Err(Error::new(r_span, "`LD reg, (nn)` only supports `A` as the destination register")),
                }
                // Ok(Self::A_IndDE)
                // Err(Error::new(r_span, "For the GameBoy, the LD A, (word) instruction was replaced by LD A, (HLD)"))
            } else if input.fork().parse::<IndHLIndex>().is_ok() {
                let hlx = input.parse()?;
                if let R::A = r {
                    Ok(Self::A_IndHLIndex(hlx))
                } else {
                    Err(Error::new(r_span, format_args!("LD reg, ({:?}) only supports A as the destination register, not {:?}", hlx, r)))
                }
            } else {
                Err(Error::new(input.span(), "Invalid LD instruction, unexpected second argument"))
            }
        } else if input.fork().parse::<RP>().is_ok() {
            let rp = input.parse()?;
            let _: Token![,] = input.parse()?;
            if let Some(_lit) = input.try_parenthesized::<LitInt>() {
                Err(Error::new(span, "For the GameBoy, the LD HL, (word) instruction was replaced by LD A, (HLI)"))
            } else if rp == RP::HL && input.fork().parse::<RP>().is_ok() {
                let rp2_span = input.span();
                let rp2 = input.parse()?;
                match rp2 {
                    RP::SP => {
                        let lah = input.lookahead1();
                        let mut positive = true;
                        if lah.peek(Token![+]) {
                            let _ = input.parse::<Token![+]>()?;
                        } else if lah.peek(Token![-]) {
                            let _ = input.parse::<Token![-]>()?;
                            positive = false;
                        } else {
                            return Err(lah.error())
                        };
                        let mut offset: SByte = input.parse()?;
                        if !positive {
                            offset = offset.invert();
                        }
                        Ok(Self::HL_SP_Offset(offset))
                    }
                    _ => Err(Error::new(rp2_span, "`LD HL, regpair + offset` must be SP")),
                }
            } else if rp == RP::SP && input.fork().parse::<RP>().is_ok() {
                let rp2_span = input.span();
                match input.parse()? {
                    RP::HL => Ok(Self::SP_HL),
                    _ => Err(Error::new(rp2_span, "`LD SP, regpair` only supports HL as the source register")),
                }
            } else {
                let lit = input.parse()?;
                Ok(Self::RP_Lit2(rp, lit))
            }
        } else if let Some(rp) = input.try_parenthesized::<RP>() {
            input.parse::<Token![,]>()?;
            let name = input.parse::<Ident>()?;
            let name_str = name.to_string().to_lowercase();
            if &name_str != "a" {
                return Err(Error::new(name.span(), "Invalid LD instruction, second param of memory write can only be register A"));
            }
            match rp {
                RP::BC => Ok(Self::IndBC_A),
                RP::DE => Ok(Self::IndDE_A),
                RP::HL => Err(Error::new(span, "Invalid LD instruction, indirect write cannot be addressed by HL")),
                RP::SP => Err(Error::new(span, "Invalid LD instruction, indirect write cannot be addressed by SP")),
            }
        } else if let Some(lit) = input.try_parenthesized::<Word>() {
            input.parse::<Token![,]>()?;
            let name = input.parse::<Ident>()?;
            let name_str = name.to_string().to_lowercase();
            match name_str.as_str() {
                "a" => Ok(Self::IndLit2_A(lit)),
                "hl" => Err(Error::new(span, "For the GameBoy, the LD (word), HL instruction was replaced with LD (HLI), A")),
                "sp" => Ok(Self::IndLit2_SP(lit)),
                _ => Err(Error::new(name.span(), "Invalid LD instruction, second param of memory write can only be register A, HL, or SP")),
            }
        } else if input.fork().parse::<IndHLIndex>().is_ok() {
            let hlx = input.parse()?;
            let _: Token![,] = input.parse()?;
            let r_span = input.span();
            let r = input.parse()?;
            if let R::A = r {
                Ok(Self::IndHLIndex_A(hlx))
            } else {
                Err(Error::new(r_span, format_args!("LD ({:?}), reg only supports A as the source register, not {:?}", hlx, r)))
            }
        } else {
            Err(Error::new(input.span(), "Invalid LD instruction, expected a register or indirect location as the first argument"))
        }
    }
}

impl Ld {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self {
            Self::A_IndBC => single_byte(span, 0x0a),
            Self::A_IndDE => single_byte(span, 0x1a),
            Self::A_IndHLIndex(IndHLIndex::HLD) => single_byte(span, 0x3a),
            Self::A_IndHLIndex(IndHLIndex::HLI) => single_byte(span, 0x2a),
            Self::A_IndLit2(l) => Ok(quote_spanned!(span=> 0xfa, #l )),
            Self::HL_SP_Offset(l) => Ok(quote_spanned!(span=> 0xf8, #l )),
            Self::IndBC_A => single_byte(span, 0x02),
            Self::IndDE_A => single_byte(span, 0x12),
            Self::IndHLIndex_A(IndHLIndex::HLD) => single_byte(span, 0x32),
            Self::IndHLIndex_A(IndHLIndex::HLI) => single_byte(span, 0x22),
            Self::IndLit2_A(l) => Ok(quote_spanned!(span=> 0xea, #l )),
            Self::IndLit2_SP(l) => Ok(quote_spanned!(span=> 0x08, #l )),
            Self::RP_Lit2(pair, lit) => {
                let opcode_val = match pair {
                    RP::BC => 0x01,
                    RP::DE => 0x11,
                    RP::HL => 0x21,
                    RP::SP => 0x31,
                };
                let opcode = LitInt::new_byte(opcode_val, span);
                Ok(quote_spanned!(span=> #opcode, #lit ))
            }
            Self::R_Lit1(r, lit) => {
                let opcode_val = match r {
                    R::B => 0x06,
                    R::C => 0x0e,
                    R::D => 0x16,
                    R::E => 0x1e,
                    R::H => 0x26,
                    R::L => 0x2e,
                    R::IndHL => 0x36,
                    R::A => 0x3e,
                };
                let opcode = LitInt::new_byte(opcode_val, span);
                Ok(quote_spanned!(span=> #opcode, #lit ))
            }
            Self::R_R(left, right) => {
                let base_val = match left {
                    R::B => 0x40,
                    R::C => 0x48,
                    R::D => 0x50,
                    R::E => 0x58,
                    R::H => 0x60,
                    R::L => 0x68,
                    R::IndHL => 0x70,
                    R::A => 0x78,
                };
                let offset_val = match right {
                    R::B => 0,
                    R::C => 1,
                    R::D => 2,
                    R::E => 3,
                    R::H => 4,
                    R::L => 5,
                    R::IndHL => 6,
                    R::A => 7,
                };
                let opcode_val = base_val + offset_val;
                if opcode_val == 0x76 {
                    panic!("LD (HL), (HL) is invalid (replaced by HALT). This should have been caught earlier in parsing.");
                }
                single_byte(span, opcode_val)
            }
            Self::SP_HL => single_byte(span, 0xf9),
        }
    }
}

#[derive(Clone, Debug)]
enum Ldh {
    A_IndC,
    A_IndLit1(UByte),
    IndC_A,
    IndLit1_A(UByte),
}

impl Parse for Ldh {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        if let Some(lit) = input.try_parenthesized::<UByte>() {
            let _: Token![,] = input.parse()?;
            let r_span = input.span();
            match input.parse()? {
                R::A => Ok(Self::IndLit1_A(lit)),
                r => Err(Error::new(r_span, format_args!("`LDH (n), reg` instruction must use A as the source register, not {:?}", r))),
            }
        } else if let Some(r) = input.try_parenthesized::<R>() {
            let _: Token![,] = input.parse()?;
            let r_span = input.span();
            match (r, input.parse()?) {
                (R::C, R::A) => Ok(Self::IndC_A),
                (R::C, _) => Err(Error::new(r_span, "`LDH (C), reg` instruction must use A as the source register")),
                (_, _) => Err(Error::new(span, "`LDH (reg), A` instruction must use C as the indirect register")),
            }
        } else if input.fork().parse::<R>().is_ok() {
            let r = input.parse()?;
            let _: Token![,] = input.parse()?;
            let r2_span = input.span();
            if let Some(lit) = input.try_parenthesized::<UByte>() {
                match r {
                    R::A => Ok(Self::A_IndLit1(lit)),
                    _ => Err(Error::new(span, "`LDH reg, (n) instruction must use A as the destination register")),
                }
            } else if let Some(r2) = input.try_parenthesized::<R>() {
                match (r, r2) {
                    (R::A, R::C) => Ok(Self::A_IndC),
                    (R::A, _) => Err(Error::new(r2_span, "`LDH A, (r)` can only use `C` as the indirect register")),
                    (_, _) => Err(Error::new(span, "`LDH r, (_)` can only use `A` as the destination register")),
                }
            } else {
                Err(Error::new(r2_span, "Expected `(n)` or `(C)` for `LDH A, _` instruction"))
            }
        } else {
            Err(Error::new(span, "Invalid `LDH` instruction, expected one of `LDH A, (n)`, `LDH (n)"))
        }
    }
}

impl Ldh {
    fn to_lit_tokens(self, span: Span) -> Result<TokenStream> {
        match self {
            Self::A_IndC => single_byte(span, 0xf2),
            Self::A_IndLit1(l) => Ok(quote_spanned!(span=> 0xf0, #l )),
            Self::IndC_A => single_byte(span, 0xe2),
            Self::IndLit1_A(l) => Ok(quote_spanned!(span=> 0xe0, #l )),
        }
    }
}

#[derive(Clone, Debug)]
enum Instr {
    Add(Add),
    Adc(RegOrLit1),
    And(RegOrLit1),
    Bit(BitCommaReg),
    Call(Call),
    Ccf,
    Cp(RegOrLit1),
    Cpl,
    Daa,
    Dec(Dec),
    Di,
    Ei,
    Halt,
    Inc(Inc),
    Jp(Jp),
    Jr(Jr),
    Ld(Ld),
    Ldh(Ldh),
    Nop,
    Or(RegOrLit1),
    Pop(RP2),
    Push(RP2),
    Ret(Ret),
    Reti,
    Res(BitCommaReg),
    Rst(Rst),
    Rl(R),
    Rla,
    Rlc(R),
    Rlca,
    Rr(R),
    Rra,
    Rrc(R),
    Rrca,
    Sbc(RegOrLit1),
    Scf,
    Set(BitCommaReg),
    Sla(R),
    Sra(R),
    Srl(R),
    Stop,
    Sub(RegOrLit1),
    Swap(R),
    Xor(RegOrLit1),
}

#[derive(Clone, Debug)]
struct Instruction {
    op_span: Span,
    details: Instr,
}

trait ParseExt {
    fn try_parenthesized<T: Parse>(self) -> Option<T>;
}

impl ParseExt for ParseStream<'_> {
    fn try_parenthesized<T: Parse>(self) -> Option<T> {
        fn inner<T: Parse>(input: ParseStream<'_>) -> Result<T> {
            let content;
            parenthesized!(content in input);
            content.parse::<T>()
        }
        if inner::<T>(&self.fork()).is_ok() {
            inner::<T>(self).ok()
        } else {
            None
        }
    }
}

impl Parse for Instruction {
    fn parse(input: ParseStream) -> Result<Self> {
        let op_ident = input.parse::<Ident>()?;
        let span = op_ident.span();
        let op_str = op_ident.to_string().to_lowercase();
        let details = match op_str.as_str() {
            "adc" => Instr::Adc(input.parse()?),
            "add" => Instr::Add(input.parse()?),
            "and" => Instr::And(input.parse()?),
            "bit" => Instr::Bit(input.parse()?),
            "call" => Instr::Call(input.parse()?),
            "ccf" => Instr::Ccf,
            "cp" => Instr::Cp(input.parse()?),
            "cpl" => Instr::Cpl,
            "daa" => Instr::Daa,
            "dec" => Instr::Dec(input.parse()?),
            "di" => Instr::Di,
            "ei" => Instr::Ei,
            "halt" => Instr::Halt,
            "inc" => Instr::Inc(input.parse()?),
            "jp" => Instr::Jp(input.parse()?),
            "jr" => Instr::Jr(input.parse()?),
            "ld" => Instr::Ld(input.parse()?),
            "ldh" => Instr::Ldh(input.parse()?),
            "nop" => Instr::Nop,
            "or" => Instr::Or(input.parse()?),
            "pop" => Instr::Pop(input.parse()?),
            "push" => Instr::Push(input.parse()?),
            "res" => Instr::Res(input.parse()?),
            "ret" => Instr::Ret(input.parse()?),
            "reti" => Instr::Reti,
            "rl" => Instr::Rl(input.parse()?),
            "rla" => Instr::Rla,
            "rlc" => Instr::Rlc(input.parse()?),
            "rlca" => Instr::Rlca,
            "rr" => Instr::Rr(input.parse()?),
            "rra" => Instr::Rra,
            "rrc" => Instr::Rrc(input.parse()?),
            "rrca" => Instr::Rrca,
            "rst" => Instr::Rst(input.parse()?),
            "sbc" => Instr::Sbc(input.parse()?),
            "scf" => Instr::Scf,
            "set" => Instr::Set(input.parse()?),
            "sla" => Instr::Sla(input.parse()?),
            "sra" => Instr::Sra(input.parse()?),
            "srl" => Instr::Srl(input.parse()?),
            "stop" => Instr::Stop,
            "sub" => Instr::Sub(input.parse()?),
            "swap" => Instr::Swap(input.parse()?),
            "xor" => Instr::Xor(input.parse()?),
            op => return Err(Error::new(span, format!("TODO: handle `{}` instruction", op))),
        };
        Ok(Instruction {
            op_span: span,
            details,
        })
    }
}

trait LitIntExt {
    fn new_byte(val: u8, span: Span) -> Self;
}

impl LitIntExt for LitInt {
    fn new_byte(val: u8, span: Span) -> Self {
        Self::new(&format!("{:#04x}", val), span)
    }
}

// struct WordPair {
//     high: LitInt,
//     low: LitInt,
// }

// impl WordPair {
//     fn from_lit(lit: LitInt) -> Result<Self> {
//         let val = lit.base10_parse::<u16>()?;
//         let high_val = (val >> 8) as u8;
//         let low_val = val as u8;
//         let high = LitInt::new_byte(high_val, lit.span());
//         let low = LitInt::new_byte(low_val, lit.span());
//         Ok(Self { high, low })
//     }
// }

impl Instruction {
    fn to_lit_tokens(self) -> Result<TokenStream> {
        let single = |val: u8| { single_byte(self.op_span, val) };
        let cb_op = |base: u8, reg: R| {
            let l = LitInt::new_byte(base + reg.offset_row(), self.op_span);
            Ok(quote_spanned!(self.op_span=> 0xcb, #l))
        };

        match self.details {
            Instr::Adc(RegOrLit1::R(r)) => single(0x88 + r.offset_row()),
            Instr::Adc(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xce, #l )),
            Instr::Add(add) => add.to_lit_tokens(self.op_span),
            Instr::And(RegOrLit1::R(r)) => single(0xa0 + r.offset_row()),
            Instr::And(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xe6, #l )),
            Instr::Bit(bit_comma_reg) => bit_comma_reg.to_lit_tokens(self.op_span, 0x40),
            Instr::Call(call) => call.to_lit_tokens(self.op_span),
            Instr::Ccf => single(0x3f),
            Instr::Cp(RegOrLit1::R(r)) => single(0xb8 + r.offset_row()),
            Instr::Cp(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xfe, #l )),
            Instr::Cpl => single(0x2f),
            Instr::Daa => single(0x27),
            Instr::Dec(dec) => dec.to_lit_tokens(self.op_span),
            Instr::Di => single(0xf3),
            Instr::Ei => single(0xfb),
            Instr::Halt => single(0x76),
            Instr::Inc(inc) => inc.to_lit_tokens(self.op_span),
            Instr::Jp(jp) => jp.to_lit_tokens(self.op_span),
            Instr::Jr(jr) => jr.to_lit_tokens(self.op_span),
            Instr::Ld(ld) => ld.to_lit_tokens(self.op_span),
            Instr::Ldh(ldh) => ldh.to_lit_tokens(self.op_span),
            Instr::Nop => single(0x00),
            Instr::Or(RegOrLit1::R(r)) => single(0xb0 + r.offset_row()),
            Instr::Or(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xf6, #l )),
            Instr::Pop(rp) => single(0xc1 + 0x10 * (rp as u8)),
            Instr::Push(rp) => single(0xc5 + 0x10 * (rp as u8)),
            Instr::Res(bit_comma_reg) => bit_comma_reg.to_lit_tokens(self.op_span, 0x80),
            Instr::Ret(ret) => ret.to_lit_tokens(self.op_span),
            Instr::Reti => single(0xd9),
            Instr::Rl(r) => cb_op(0x10, r),
            Instr::Rla => single(0x17),
            Instr::Rlc(r) => cb_op(0x00, r),
            Instr::Rlca => single(0x07),
            Instr::Rr(r) => cb_op(0x18, r),
            Instr::Rra => single(0x1f),
            Instr::Rrc(r) => cb_op(0x08, r),
            Instr::Rrca => single(0x0f),
            Instr::Rst(rst) => rst.to_lit_tokens(self.op_span),
            Instr::Sbc(RegOrLit1::R(r)) => single(0x98 + r.offset_row()),
            Instr::Sbc(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xde, #l )),
            Instr::Set(bit_comma_reg) => bit_comma_reg.to_lit_tokens(self.op_span, 0xc0),
            Instr::Scf => single(0x37),
            Instr::Sla(r) => cb_op(0x20, r),
            Instr::Sra(r) => cb_op(0x28, r),
            Instr::Srl(r) => cb_op(0x38, r),
            Instr::Stop => Ok(quote_spanned!(self.op_span=> 0x10, 0x00 )),
            Instr::Sub(RegOrLit1::R(r)) => single(0x90 + r.offset_row()),
            Instr::Sub(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xd6, #l )),
            Instr::Swap(r) => cb_op(0x30, r),
            Instr::Xor(RegOrLit1::R(r)) => single(0xa8 + r.offset_row()),
            Instr::Xor(RegOrLit1::Lit1(l)) => Ok(quote_spanned!(self.op_span=> 0xee, #l )),
        }
    }
}

#[derive(Debug)]
struct InstructionList {
    instrs: Punctuated<Instruction, Token![;]>,
}

impl Parse for InstructionList {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![;]>()?;
        let instrs = Punctuated::<Instruction, Token![;]>::parse_separated_nonempty(input)?;
        Ok(Self { instrs })
    }
}

/// Usage:
///
///     #use gb_color_emu_macros::gbc_asm;
///     let rom = gbc_asm![
///         ; ld bc, 0x1234
///         ; jmp nz, 0x3456
///     ];
#[proc_macro]
pub fn gbc_asm(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = input.into();
    let parsed = parse_macro_input!(input as InstructionList);

    // let bytes = Ident::new("rom_bytes", Span::call_site());
    let instrs: Vec<TokenStream> = parsed.instrs
        .into_iter()
        .map(|instr| instr.to_lit_tokens())
        .collect::<Result<Vec<TokenStream>>>()
        .unwrap();

    let compile = quote::quote! {
        vec![ #( #instrs ),* ]
        // {
        //     let mut #bytes: Vec<u8> = vec![];
        //     #(#instrs);*;
        //     #bytes
        // }
    };
    compile.into()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
