use std::process::Command;
use std::env;
use std::vec::Vec;

fn usage()
{
    print!("Usage: run --help | PROGRAM

Run command PROGRAM with the current user's login shell. This is
useful for writing launchd plists. Due to Catalina's security
limitations, if the plists is for running a script of some interpreted
language, it is very hard to give the interpreter sufficient
permission. For example if you have a Python script, and you decide to
run it like 'zsh -l -c python /name/of/script', giving zsh and python
full disk access is not enough for the script to access files. However
if you run it by using this program, it is ok. (Full disk access is
still needed on this program, of course). In fact if you do it without
giving it permission, macOS will ask you for it with a dialog box,
like with an app bundle, I have never seen with the case of directly
running 'zsh -l -c python ...', and I have no idea why.

PROGRAM is run with '$SHELL -l -c PROGRAM', so it should be a single
string.
");
}

fn main()
{
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 || args[1] == "--help"
    {
        usage();
        return;
    }

    let shell = match env::var("SHELL")
    {
        Ok(path) => path,
        Err(_) => String::from("zsh"),
    };

    let status = Command::new(&shell[..]).arg("-l").arg("-c").args(&(env::args().collect::<Vec<String>>())[1..])
        .status()
        .expect("Failed to run command");
    std::process::exit(match status.code()
    {
        Some(code) => code,
        None => -1, // Killed by a signal
    });
}
