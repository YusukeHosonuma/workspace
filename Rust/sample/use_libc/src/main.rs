extern crate libc;

fn main() {

    let cpus = unsafe {
        libc::sysconf(libc::_SC_NPROCESSORS_ONLN) as usize
    };
    let psize = unsafe {
        libc::sysconf(libc::_SC_PAGESIZE) as usize
    };

    println!("{} processors on this machine.", cpus);
    println!("This machine pagesize is {}.", psize);
}
