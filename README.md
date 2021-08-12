In conjunction with the new Elf/OS kernel release, I am releasing version 1 of my filesystem acceleration module.

Based on features added in kernel 0.3.1 it has been possible to build my filesystem code as a module that can be loaded separately on top of the running kernel. This makes it independent of any particular kernel version or build, independent of any particular hardware, and also makes it easy to enable or disable. So, this should now be able to be used on other than just Pico/Elf-like machines.

Builds later than 2 require kernel 0.4.0 or later as they use the new heap manager. Note that due to the size increase in the kernel, the "-k" option to load to kernel memory is no longer supported, as there is no free kernel memeory to load to.

To install, run from the command line as with any other Elf/OS program. By default, the installer will reserve a block of heap memory, copy the module into that block, link it into the running kernel, and then exit. To remove the module, just reboot the system.

