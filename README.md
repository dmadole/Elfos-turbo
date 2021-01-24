In conjunction with the new Elf/OS kernel release, I am releasing version 1 of my filesystem acceleration module.

Based on features added in kernel 0.3.1 it has been possible to build my filesystem code as a module that can be loaded separately on top of the running kernel. This makes it independent of any particular kernel version or build, independent of any particular hardware, and also makes it easy to enable or disable. So, this should now be able to be used on other than just Pico/Elf-like machines.

Note that a kernel version of 0.3.1 or later is a prerequisite. Mike Riley has made this available on his software page: http://www.elf-emulation.com/software.html

To install, run from the command line as with any other Elf/OS program. By default, the installer will reserve a block of high memory, copy the module into that block, link it into the running kernel, and then exit. To remove the module, just reboot the system. If you would like to have the module persistent, you can name it INIT and the normal Elf/OS mechanism will then install it at boot time.

Users should be aware as mentioned with the new kernel release that not all existing Elf/OS programs are friendly with the new memory management model. For those programs that are not, it can result in overwriting the module in memory which leads to a crash. Work will need to be done to update those existing programs for this memory management model to fully work. When loaded into high memory on a 32K RAM machine, the module will typically be at 7a00-7bff.

As an interim measure, I have provided an option to load the module into kernel memory at 1e00-1fff rather into reserved high memory. This location is safe from legacy programs that don’t yet understand high memory allocation. To load the module in this way, use the ‘-k’ option when running, that is, run as ‘turbo -k’. I do recommend using this option for now. Note that it is not possible to load as INIT in this way.
