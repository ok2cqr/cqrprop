# CQRPROP

A small application that shows propagation data from Paul, N0NBH website http://www.hamqsl.com/solar.html on a Linux desktop. I used to have Conky downloading the image but still had some issues with it. This application is very simple and easy to use.

How to install CQRPROP?
------------------------

Ubuntu 24.04 users can use my PPA on Launchpad:

```
sudo add-apt-repository ppa:ok2cqr/cqrprop
sudo apt-get update
sudo apt-get install cqrprop
```

How to build CQRPROP from source code?
---------------------------------------

Application is developed in Lazarus 3.x. You have to install Free Pascal compiler and Lazarus to build the CQRPROP.

```
sudo apt-get install git lazarus-ide lcl lcl-gtk2 lcl-nogui lcl-units lcl-utils lazarus lazarus-doc lazarus-src \
         fp-units-misc fp-units-rtl fp-utils fpc fpc-source libssl-dev
git clone https://github.com/ok2cqr/cqrprop.git
cd cqrprop
make 
sudo make install 
```


Screenshots:
------------

![Alt text](http://www.ok2cqr.com/linux/images/cqrprop/cqrprop.png "Main window")

![Alt text](http://www.ok2cqr.com/linux/images/cqrprop/cqrprop_options.png "Options window")
