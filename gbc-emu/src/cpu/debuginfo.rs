use dynasmrt::AssemblyOffset;
use gimli::write::{
    Address, AttributeValue as AV, CallFrameInstruction as CFI,
    CommonInformationEntry as CIE, DwarfUnit, EndianVec, Expression,
    FrameDescriptionEntry as FDE, FrameTable, LineProgram, LineString,
    Sections, UnitEntryId,
};
use std::{fmt, pin::Pin, str::FromStr};
use target_lexicon::triple;
use thiserror::Error;

use crate::system::SystemRef;
// use super::JitAssembler;

// const STACK_SIZE_BYTES: u8 = JitAssembler::STACK_SIZE_BYTES;

#[derive(Clone, Debug)]
pub(crate) struct JitAssemblerDebugInfo {
    start_pc: u16,
    epilogue_offset: Option<AssemblyOffset>,
    lines: Vec<LineDebugInfo>,
}

#[derive(Clone, Debug)]
struct LineDebugInfo {
    pc: u16,
    offset: AssemblyOffset,
    source: String,
}

#[derive(Error, Debug)]
pub(crate) enum DebugInfoError {
    #[error("failed to build ELF for debug info: {0}")]
    FaerieError(#[from] faerie::artifact::ArtifactError),
    #[error("failed to build debug info: {0}")]
    GimliError(#[from] gimli::write::Error),
    #[error("failed to write source for debug info: {0}")]
    IoError(#[from] std::io::Error),
    #[error("no source lines were added for debugging")]
    Empty,
}

impl JitAssemblerDebugInfo {
    pub(crate) fn new(start_pc: u16) -> Self {
        Self {
            start_pc,
            epilogue_offset: None,
            lines: vec![],
        }
    }

    #[inline]
    pub(crate) fn set_epilogue_offset(&mut self, offset: AssemblyOffset) {
        self.epilogue_offset = Some(offset);
    }

    #[inline]
    pub(crate) fn add_line(&mut self, pc: u16, offset: AssemblyOffset, instr: impl std::fmt::Display, op_1: u8, op_2: Option<u8>, op_3: Option<u8>) {
        let source = match (op_2, op_3) {
            (None, None) => format!("<{:04x}>: {:02x}            {}", pc, op_1, instr),
            (Some(x), None) => format!("<{:04x}>: {:02x} {:02x}         {}", pc, op_1, x, instr),
            (Some(x), Some(y)) => format!("<{:04x}>: {:02x} {:02x} {:02x}      {}", pc, op_1, x, y, instr),
            (None, Some(y)) => format!("<{:04x}>: {:02x} {:02x}         {}", pc, op_1, y, instr),
        };
        if self.lines.is_empty() {
            debug!("Jitting instructions starting at {:#06x}", self.start_pc);
        }
        debug!("Jitting instruction {}", source);
        self.lines.push(LineDebugInfo {
            pc,
            offset,
            source,
        });
    }

    // pub(crate) fn last_offset(&self) -> Option<AssemblyOffset> {
    //     let line = self.lines.last()?;
    //     Some(line.offset)
    // }

    pub(crate) fn compile(self, func: &super::OwnedJittedFunc) -> Result<DebugObjFile, DebugInfoError> {
        if self.lines.is_empty() {
            return Err(DebugInfoError::Empty);
        }

        let base_pc = self.start_pc;
        let base_ptr = func.buffer.ptr(func.offset) as u64;
        let end_ptr = base_ptr + (func.buffer.len() as u64);

        let filename = format!("jit-source-{:#06x}_", base_pc);
        let mut source = String::new();

        // let mut f = tempfile::Builder::new()
        //     .prefix(&filename)
        //     .suffix(".txt")
        //     .tempfile()?;
        // {
        //     use std::io::Write;

        //     let mut writer = std::io::BufWriter::new(&mut f);
        //     for line in self.lines.iter() {
        //         trace!("Line of source\t|\t{}", line.source);
        //         writeln!(&mut writer, "{}", line.source).unwrap();
        //     }
        //     writer.flush()?;
        // }

        let encoding = gimli::Encoding {
            format: gimli::Format::Dwarf32,
            version: 5,
            address_size: 8,
        };
        let mut dwarf = DwarfUnit::new(encoding);

        let file_path = std::path::Path::new(&filename);
        let comp_dir_id = dwarf.strings.add(file_path.parent().unwrap().to_str().unwrap());
        let comp_file_id = dwarf.strings.add(file_path.file_name().unwrap().to_str().unwrap());

        // let range_list = RangeList(vec![Range::StartLength {
        //     begin: Address::Constant(base_ptr),
        //     length: func.buffer.size() as u64,
        // }]);
        // let range_list_id = dwarf.unit.ranges.add(range_list);
        let root_id = dwarf.unit.root();

        let line_program = LineProgram::new(
            encoding,
            gimli::LineEncoding {
                minimum_instruction_length: 1,
                maximum_operations_per_instruction: 1,
                default_is_stmt: true,
                line_base: -3,
                line_range: 6,
            },
            LineString::StringRef(comp_dir_id),
            LineString::StringRef(comp_file_id),
            None,
        );

        dwarf.unit.line_program = line_program;

        // See rustc_codegen_cranelift/src/debuginfo/line_info.rs fn create_debug_lines
        {
            let line_program = &mut dwarf.unit.line_program;
            // debug!("Starting line_program sequence at {:p}", base_ptr as *const u8);
            line_program.begin_sequence(Some(Address::Constant(base_ptr)));

            // let mut locations = LocationList(Vec::with_capacity(self.lines.len() + 1));
            // locations.0.push(Location::BaseAddress {
            //     address: Address::Constant(base_ptr),
            // });
            let mut last_offset: Option<u64> = None;
            for (i, line_info) in self.lines.iter().enumerate() {
                trace!("Debug info offset #{}: addr={:#06x}, offset={:?}", i, line_info.pc, line_info.offset);
                source.push_str(&line_info.source);
                source.push('\n');
                let this_offset = line_info.offset.0.try_into().unwrap();
                if let Some(last_offset) = last_offset {
                    if this_offset == last_offset {
                        line_program.generate_row();
                        continue;
                    }
                }
                last_offset = Some(this_offset);
                line_program.row().line = i as u64 + 1;
                line_program.row().column = 0;
                line_program.generate_row();
                trace!("Done with debug info row #{}", i);
            }

            line_program.end_sequence(u64::from(end_ptr - base_ptr));
        }

        {
            let root = dwarf.unit.get_mut(root_id);
            // root.set(gimli::DW_AT_stmt_list, AV::LineStringRef(source_line_id));
            root.set(gimli::DW_AT_low_pc, AV::Address(Address::Constant(base_ptr)));
            root.set(gimli::DW_AT_high_pc, AV::Address(Address::Constant(end_ptr)));
            root.set(gimli::DW_AT_name, AV::StringRef(comp_file_id));
            root.set(gimli::DW_AT_comp_dir, AV::StringRef(comp_dir_id));
            let producer_id = dwarf.strings.add(format!("gb-color-emu-hl JIT v0.0"));
            root.set(gimli::DW_AT_producer, AV::StringRef(producer_id));
            root.set(gimli::DW_AT_main_subprogram, AV::Flag(true));
            // root.set(
            //     gimli::DW_AT_ranges,
            //     AV::RangeListRef(range_list_id),
            // );
            root.set(gimli::DW_AT_entry_pc, AV::Address(Address::Constant(base_ptr)));
        }

        let debug_types = Self::add_types(&mut dwarf);
        let subprogram = dwarf.unit.add(root_id, gimli::DW_TAG_subprogram);
        {
            let file_id = dwarf.unit.line_program.add_file(
                LineString::StringRef(comp_file_id),
                dwarf.unit.line_program.default_directory(),
                None,
            );
            let die = dwarf.unit.get_mut(subprogram);
            die.set(gimli::DW_AT_low_pc, AV::Address(Address::Constant(base_ptr)));
            die.set(gimli::DW_AT_high_pc, AV::Address(Address::Constant(end_ptr)));
            let fn_name_id = dwarf.strings.add(
                format!("<basic block starting at GBC PC {:#06x}>", base_pc),
            );
            die.set(gimli::DW_AT_name, AV::StringRef(fn_name_id));
            die.set(gimli::DW_AT_calling_convention, AV::CallingConvention(gimli::DW_CC_normal));
            die.set(gimli::DW_AT_type, AV::UnitRef(debug_types.unsigned_32));
            die.set(gimli::DW_AT_decl_file, AV::FileIndex(Some(file_id)));
            die.set(gimli::DW_AT_decl_line, AV::Udata(1));
            let mut frame_base_loc = Expression::new();
            frame_base_loc.op(gimli::DW_OP_reg7); // reg7 is rsp
            // frame_base_loc.op(gimli::DW_OP_const1u);
            // frame_base_loc.op(gimli::DwOp(32));
            // frame_base_loc.op(gimli::DW_OP_plus);
            die.set(gimli::DW_AT_frame_base, AV::Exprloc(frame_base_loc));
        }

        let mut frame_table = FrameTable::default();
        const RBP: gimli::Register = gimli::Register(6);
        const RSP: gimli::Register = gimli::Register(7);
        let cie_id = {
            let cie_encoding = gimli::Encoding {
                format: gimli::Format::Dwarf32,
                version: 1,
                address_size: 8,
            };
            let mut cie = CIE::new(cie_encoding, 1, -8, gimli::Register(16));
            cie.add_instruction(CFI::Cfa(RSP, 0));
            frame_table.add_cie(cie)
        };
        let _fde_id = {
            let mut fde = FDE::new(Address::Constant(base_ptr), func.buffer.len().try_into().unwrap());
            fde.add_instruction(1, CFI::Cfa(RSP, 8));
            fde.add_instruction(4, CFI::Cfa(RBP, 8));
            // let offset = self.lines[0].offset.0 as _;
            // fde.add_instruction(offset, CFI::Cfa(RBP, 0));
            // fde.add_instruction(offset + 0x10, CFI::Cfa(RBP, -8));
            // fde.add_instruction(offset + 0x20, CFI::Cfa(RBP, -8));
            frame_table.add_fde(cie_id, fde)
        };

        let param_id = dwarf.unit.add(subprogram, gimli::DW_TAG_formal_parameter);
        {
            let die = dwarf.unit.get_mut(param_id);
            let name_id = dwarf.strings.add("jit_data_ptr");
            die.set(gimli::DW_AT_name, AV::StringRef(name_id));
            die.set(gimli::DW_AT_type, AV::UnitRef(debug_types.ref_mut_jitdata));
            let mut location = Expression::new();
            location.op_breg(RBP, 0);
            die.set(gimli::DW_AT_location, AV::Exprloc(location));
        }
        let regsptr_id = dwarf.unit.add(subprogram, gimli::DW_TAG_variable);
        {
            let die = dwarf.unit.get_mut(regsptr_id);
            let name_id = dwarf.strings.add("regs_ptr");
            die.set(gimli::DW_AT_name, AV::StringRef(name_id));
            die.set(gimli::DW_AT_type, AV::UnitRef(debug_types.ref_mut_registers));
            let mut location = Expression::new();
            location.op_breg(RBP, 0);
            location.op_deref();
            if memoffset::offset_of!(SystemRef, cpu) != 0 {
                location.op_constu(memoffset::offset_of!(SystemRef, cpu) as _);
                location.op(gimli::DW_OP_plus);
            }
            die.set(gimli::DW_AT_location, AV::Exprloc(location));
        }

        let pc_range = (
            self.lines[0].pc,
            self.lines[self.lines.len() - 1].pc,
        );
        DebugObjFile::new(dwarf, frame_table, None, source, pc_range)
    }

    fn add_types(dwarf: &mut DwarfUnit) -> DebugTypeRefs {
        let root_id = dwarf.unit.root();

        let u8_type_id = dwarf.unit.add(root_id, gimli::DW_TAG_base_type);
        let u16_type_id = dwarf.unit.add(root_id, gimli::DW_TAG_base_type);
        let u32_type_id = dwarf.unit.add(root_id, gimli::DW_TAG_base_type);
        let jitdata_id = dwarf.unit.add(root_id, gimli::DW_TAG_structure_type);
        let _jitdata_regs = dwarf.unit.add(jitdata_id, gimli::DW_TAG_member);
        let _jitdata_cart = dwarf.unit.add(jitdata_id, gimli::DW_TAG_member);
        let _jitdata_memory = dwarf.unit.add(jitdata_id, gimli::DW_TAG_member);
        let ref_mut_jitdata_id = dwarf.unit.add(root_id, gimli::DW_TAG_pointer_type);
        let void_ptr_id = dwarf.unit.add(root_id, gimli::DW_TAG_pointer_type);
        let registers_id = dwarf.unit.add(root_id, gimli::DW_TAG_structure_type);
        let ref_mut_registers_id = dwarf.unit.add(root_id, gimli::DW_TAG_pointer_type);

        fn def_unsigned(dwarf: &mut DwarfUnit, id: UnitEntryId, name: &str, size: u8) {
            let die = dwarf.unit.get_mut(id);
            let type_name_id = dwarf.strings.add(name);
            die.set(gimli::DW_AT_name, AV::StringRef(type_name_id));
            die.set(gimli::DW_AT_encoding, AV::Encoding(gimli::DW_ATE_unsigned));
            die.set(gimli::DW_AT_byte_size, AV::Data1(size));
        }

        def_unsigned(dwarf, u8_type_id, "u8", 1);
        def_unsigned(dwarf, u16_type_id, "u16", 2);
        def_unsigned(dwarf, u32_type_id, "u32", 4);

        fn def_ptr(dwarf: &mut DwarfUnit, id: UnitEntryId, name: &str, target: Option<UnitEntryId>) {
            let die = dwarf.unit.get_mut(id);
            let name_id = dwarf.strings.add(name);
            die.set(gimli::DW_AT_name, AV::StringRef(name_id));
            if let Some(t) = target {
                die.set(gimli::DW_AT_type, AV::UnitRef(t));
            }
        }

        def_ptr(dwarf, void_ptr_id, "*mut void", None);

        fn def_struct(dwarf: &mut DwarfUnit, id: UnitEntryId, name: &str, fields: &[(&str, u8, UnitEntryId)]) {
            let die = dwarf.unit.get_mut(id);
            let name_id = dwarf.strings.add(name);
            die.set(gimli::DW_AT_name, AV::StringRef(name_id));

            let mut offset = 0u8;
            for (field_name, size, field_type_id) in fields.iter().copied() {
                let field_id = dwarf.unit.add(id, gimli::DW_TAG_member);
                let die = dwarf.unit.get_mut(field_id);
                let field_name_id = dwarf.strings.add(field_name);
                die.set(gimli::DW_AT_name, AV::StringRef(field_name_id));
                die.set(gimli::DW_AT_type, AV::UnitRef(field_type_id));
                die.set(gimli::DW_AT_data_member_location, AV::Data1(offset));
                offset = offset.checked_add(size).unwrap();
            }

            let die = dwarf.unit.get_mut(id);
            die.set(gimli::DW_AT_byte_size, AV::Data1(offset));
        }

        debug_assert_eq!(std::mem::size_of::<super::Registers>(), 16);
        debug_assert_eq!(std::mem::align_of::<super::Registers>(), 2);
        def_struct(dwarf, registers_id, "Registers", &[
            ("a", 1, u8_type_id),
            ("f", 1, u8_type_id),
            ("b", 1, u8_type_id),
            ("c", 1, u8_type_id),
            ("d", 1, u8_type_id),
            ("e", 1, u8_type_id),
            ("h", 1, u8_type_id),
            ("l", 1, u8_type_id),
            ("sp", 2, u16_type_id),
            ("pc", 2, u16_type_id),
            ("ie", 1, u8_type_id),
            ("if", 1, u8_type_id),
            ("ime", 1, u8_type_id),
        ]);

        def_ptr(dwarf, ref_mut_registers_id, "&mut Registers", Some(registers_id));

        debug_assert_eq!(std::mem::size_of::<SystemRef>(), 8 * 6);
        def_struct(dwarf, jitdata_id, "SystemRef", &[
            ("cart", 8, void_ptr_id),
            ("cpu_regs", 8, ref_mut_registers_id),
            ("lcd", 8, void_ptr_id),
            ("memory", 8, void_ptr_id),
            ("screen", 8, void_ptr_id),
            ("sound", 8, void_ptr_id),
        ]);

        def_ptr(dwarf, ref_mut_jitdata_id, "&mut SystemRef", Some(jitdata_id));

        DebugTypeRefs {
            unsigned_32: u32_type_id,
            ref_mut_jitdata: ref_mut_jitdata_id,
            ref_mut_registers: ref_mut_registers_id,
        }
    }
}

struct DebugTypeRefs {
    unsigned_32: UnitEntryId,
    ref_mut_jitdata: UnitEntryId,
    ref_mut_registers: UnitEntryId,
}

pub(crate) struct DebugObjFile {
    pub(crate) binary: Pin<Box<[u8]>>,
    source_file: Option<tempfile::NamedTempFile>,
    source: String,
    pc_range: (u16, u16),
}

impl fmt::Debug for DebugObjFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("DebugObjFile")
            .field("binary", &format_args!("<{} bytes at {:p}>", self.binary.len(), self.binary.as_ptr()))
            .field("source_file", &self.source_file.as_ref())
            // .field("source", &self.source)
            .field("start_pc", &format_args!("{:#06x}", self.pc_range.0))
            .field("end_pc", &format_args!("{:#06x}", self.pc_range.1))
            .finish()
    }
}

impl Drop for DebugObjFile {
    fn drop(&mut self) {
        crate::jit_debug::unregister_code(&self);
        // std::mem::forget(self.source_file.take());
    }
}

impl DebugObjFile {
    fn new(
        mut dwarf: gimli::write::DwarfUnit,
        frame_table: FrameTable,
        source_file: Option<tempfile::NamedTempFile>,
        source: String,
        pc_range: (u16, u16),
    ) -> Result<DebugObjFile, DebugInfoError>
    {
        let triple = triple!("x86_64-unknown-unknown-unknown-elf");
        let mut artifact = faerie::ArtifactBuilder::new(triple)
            .name("jitted-function.o".into())
            .library(true)
            .finish();

        let mut sections = Sections::new(EndianVec::new(gimli::LittleEndian));
        dwarf.write(&mut sections)?;
        frame_table.write_debug_frame(&mut sections.debug_frame)?;
        frame_table.write_eh_frame(&mut sections.eh_frame)?;
        sections.for_each::<_, DebugInfoError>(|id, data| {
            let name = id.name();
            artifact.declare(name, faerie::Decl::section(faerie::SectionKind::Debug))?;
            artifact.define(name, data.slice().to_vec())?;
            Ok(())
        })?;

        let elf_blob = artifact.emit()?;

        // {
        //     use std::io::Write;
        //     const FILENAME: &str = "/home/cassie/projects/gb-color-emu/jitted-function.o";
        //     info!("Writing debug ELF file to `{}`", FILENAME);
        //     let mut f = std::fs::File::create(FILENAME)?;
        //     f.write_all(&elf_blob)?;
        // }

        let symbol_file: Pin<Box<[u8]>> = Pin::new(elf_blob.into_boxed_slice());
        let obj = DebugObjFile {
            binary: symbol_file,
            source_file,
            source,
            pc_range,
        };
        crate::jit_debug::register_code(&obj);
        Ok(obj)
    }
}
