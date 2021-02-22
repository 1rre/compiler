# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "tjm1518/void"
  config.vm.allow_fstab_modification = false
  config.vm.provision "shell", inline: <<-SHELL
    # Standard build tools
    sudo xbps-install -Suy gcc gdb make dos2unix git

    # Erlang runtime system
    sudo xbps-install -y erlang
    
    # MIPS cross compiler
    sudo xbps-install -y cross-mips-linux-musl
    echo "alias mips-linux-gnu-gcc='mips-linux-musl-gcc'\nexport mips-linux-gnu-gcc\n" > ~/.profile
    # QEMU Emulator
    sudo xbps-install -y qemu

  SHELL
end
