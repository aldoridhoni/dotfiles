#!/bin/sh

font_path='/usr/lib/kbd/consolefont'

## Fedora system
sudo dracut --force --verbose --kernel-cmdline rd.vconsole.font=ter-powerline-v18b.psf.gz

## Arch system
