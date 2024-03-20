use std::{pin::Pin, sync::{Mutex, MutexGuard}};

use crate::cpu::debuginfo::DebugObjFile;

#[repr(u32)]
#[derive(Copy, Clone, Debug)]
enum GdbJitAction {
    NoAction = 0,
    RegisterFn,
    UnregisterFn,
}

#[repr(C)]
#[derive(Clone, Debug)]
struct GdbJitCodeEntry {
    next_entry: *mut GdbJitCodeEntry,
    prev_entry: *mut GdbJitCodeEntry,
    symfile_addr: *const u8,
    symfile_size: u64,
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct GdbJitDescriptor {
    version: u32,
    action_flag: GdbJitAction,
    relevant_entry: *mut GdbJitCodeEntry,
    first_entry: *mut GdbJitCodeEntry,
}

unsafe impl Sync for GdbJitDescriptor {}


/// Hook for GDB to register debug info when a debugger is attached.
///
/// Safety: You must be holding the `JIT_DEBUG_DESCRIPTOR_LOCK` lock.
#[no_mangle]
#[inline(never)]
unsafe extern "C" fn __jit_debug_register_code(_guard: &MutexGuard<JitDebugDescriptorHandle>) {}

struct JitDebugDescriptorHandle;

lazy_static::lazy_static! {
    static ref JIT_DEBUG_DESCRIPTOR_LOCK: Mutex<JitDebugDescriptorHandle> = Mutex::new(JitDebugDescriptorHandle);
}

#[no_mangle]
pub static mut __jit_debug_descriptor: GdbJitDescriptor = GdbJitDescriptor {
    version: 1,
    action_flag: GdbJitAction::NoAction,
    relevant_entry: std::ptr::null_mut(),
    first_entry: std::ptr::null_mut(),
};

// Safety: `__jit_debug_descriptor` is only ever changed while the
// `JIT_DEBUG_DESCRIPTOR_LOCK` is held. GDB may make changes on its own, but it
// only does so when the thread holding the lock calls
// `__jit_debug_register_code()`.
pub(crate) fn register_code(obj_file: &DebugObjFile) {
    let symbol_file = &obj_file.binary;
    debug!("Registering JIT symbol file at {:p} (from {:?})", symbol_file.as_ptr(), obj_file);

    // 1: Create a code entry for the file, which gives the start and size of
    //    the symbol file.
    // 2: Add it to the linked list in the JIT descriptor.
    // 3: Point the `relevant_entry` field of the descriptor to the entry.
    // 4: Set `action_flag` to `JIT_REGISTER` and call `__jit_debug_register_code`.

    let guard = JIT_DEBUG_DESCRIPTOR_LOCK.lock().unwrap();

    let prev_first_entry_ptr = unsafe { __jit_debug_descriptor.first_entry };
    let code_entry = GdbJitCodeEntry {
        next_entry: prev_first_entry_ptr,
        prev_entry: std::ptr::null_mut(),
        symfile_addr: symbol_file.as_ptr(),
        symfile_size: symbol_file.len().try_into().unwrap(),
    };
    let code_entry: *mut GdbJitCodeEntry = Box::into_raw(Box::new(code_entry));
    unsafe {
        if let Some(prev_first_entry) = prev_first_entry_ptr.as_mut() {
            debug_assert!(prev_first_entry.prev_entry.is_null());
            prev_first_entry.prev_entry = code_entry;
        }
        __jit_debug_descriptor.first_entry = code_entry;
        __jit_debug_descriptor.relevant_entry = code_entry;
        __jit_debug_descriptor.action_flag = GdbJitAction::RegisterFn;
        __jit_debug_register_code(&guard);
    }
}

pub(crate) fn unregister_code(obj_file: &DebugObjFile) {
    let symbol_file = &obj_file.binary;
    let symbol_file_ptr = symbol_file.as_ptr();
    debug!("Unregistering JIT symbol file at {:p} (from {:?})", symbol_file_ptr, obj_file);

    // 1: Remove the code entry corresponding to the code from the linked list.
    // 2: Point the `relevant_entry` field of the descriptor at the code entry.
    // 3: Set `action_flag` to `JIT_UNREGISTER` and call `__jit_debug_register_code`.
    let guard = JIT_DEBUG_DESCRIPTOR_LOCK.lock().unwrap();

    unsafe {
        // Loop through the linked list and find the matching entry.
        let mut entry_ptr = __jit_debug_descriptor.first_entry;
        while !entry_ptr.is_null() {
            let entry = &mut *entry_ptr;
            trace!("Looking at JIT code entry: {:?}", entry);
            let slice_ptr = (*entry_ptr).symfile_addr;
            if slice_ptr == symbol_file_ptr {
                debug_assert_eq!(symbol_file.len(), (*entry_ptr).symfile_size.try_into().unwrap());
                break;
            }
            entry_ptr = (*entry_ptr).next_entry;
        }

        let entry: &mut GdbJitCodeEntry = match entry_ptr.as_mut() {
            None => {
                warn!("Failed to unregister symbol file at {:p}", symbol_file_ptr);
                return;
            }
            Some(e) => e,
        };

        if let Some(prev) = entry.prev_entry.as_mut() {
            debug_assert_eq!(
                prev.next_entry, entry as *mut GdbJitCodeEntry,
                "Expected previous entry to point to {:p}! {:?}",
                entry as *mut GdbJitCodeEntry, prev,
            );
            prev.next_entry = entry.next_entry;
        }
        if let Some(next) = entry.next_entry.as_mut() {
            debug_assert_eq!(
                next.prev_entry, entry as *mut GdbJitCodeEntry,
                "Expected next_entry to point to {:p}! {:?}",
                entry as *mut GdbJitCodeEntry, next,
            );
            next.prev_entry = entry.prev_entry;
        }

        if __jit_debug_descriptor.first_entry == entry {
            __jit_debug_descriptor.first_entry = entry.next_entry;
        }
        __jit_debug_descriptor.relevant_entry = entry;
        __jit_debug_descriptor.action_flag = GdbJitAction::UnregisterFn;
        __jit_debug_register_code(&guard);

        let _entry = Box::from_raw(entry);
    }
}
