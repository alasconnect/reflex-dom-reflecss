#!/bin/bash
set -e -u -x

function os_bootstrap
{
  export LANGUAGE=en_US.UTF-8
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8
  sudo locale-gen en_US.UTF-8
  sudo dpkg-reconfigure locales -f noninteractive
  sudo apt-get update
  sudo apt-get install -y git unzip automake autoconf libreadline-dev libncurses5-dev libssl-dev libyaml-dev libxslt-dev libffi-dev libtool unixodbc-dev build-essential inotify-tools bash-completion
}

function reflex_platform_bootstrap
{
  git clone https://github.com/reflex-frp/reflex-platform
  sudo ln -s ~/reflex-platform/try-reflex /usr/local/bin/try-reflex
  echo "export PATH=/home/vagrant/reflex-platform:/home/vagrant/reflex-platform/scripts:$PATH" >> /home/vagrant/.bashrc
}

function sass_bootstrap
{
  wget https://github.com/sass/dart-sass/releases/download/1.20.1/dart-sass-1.20.1-linux-x64.tar.gz -P ~/
  tar -xvf ~/dart-sass-1.20.1-linux-x64.tar.gz --directory ~/
  sudo mv ~/dart-sass/* /usr/local/bin
  rm -rf ~/dart-sass
  rm ~/dart-sass-1.20.1-linux-x64.tar.gz
}

os_bootstrap
reflex_platform_bootstrap
sass_bootstrap
