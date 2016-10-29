// システムコールのサンプル

fn main() {
    let cpus = get_cpus();
    println!("{} processors on machine.", cpus);
}

#[cfg(windows)] // Windowsに限定（Macではビルドできない）
fn get_cpus() -> usize {

    #[repr(C)] // そのプラットフォーム上のC構造体のレイアウトと互換性を保証する
    struct SYSTEM_INFO {
        wProcessorArchitecture: u16,
        wReserved: u16,
        dwPageSize: u32,
        lpMinimumApplicationAddress: *mut u8,
        lpMaximumApplicationAddress: *mut u8,
        dwActiveProcessorMask: *mut u8,
        dwNumberOfProcessors: u32,
        dwProcessorType: u32,
        dwAllocationGranularity: u32,
        wProcessorLevel: u16,
        wProcessorRevision: u16,
    }

    // 他言語で書かれた関数をABIを利用して呼び出す
    extern "system" {
        fn GetSystemInfo(lpSystemInfo: *mut SYSTEM_INFO);
    }

    unsafe {
        let mut sysinfo: SYSTEM_INFO = std::mem::uninitialized();
        GetSystemInfo(&mut sysinfo);
        sysinfo.dwNumberOfProcessors as usize
    }
}
