                               _
__   _____ _ __ __ _  ___ _ __| |
\ \ / / _ \ '__/ _` |/ _ \ '__| |
 \ V /  __/ | | (_| |  __/ |  | |
  \_/ \___|_|  \__,_|\___|_|  |_|
------

The `veraerl` component is meant to provide a simple Erlang interface to the Vera suite of Home Automation devices. All of my testing will be done on a Vera 3 running MiOS UI7 as that is all I have.

## Supported Queries

### device listing

The `vera_client:device_list/1` method may be used to request a simple ID/name listing of devices on the Vera.

### discovery

The `veraerl_util/0` method should report any Vera devices associated with your current Public IP address.

### device id

The `vera_client:device_id/2` method may be used to request the integer ID of a device, if it exists on the Vera.

### device variables

The `vera_client:device_vars/2` method will list all the variables (and according service) for a given device, if it exists on the Vera.

### device power

The `vera_client:device_power/3` method will likely change into something more uPnP service aware. In the mean time, it will attempt to switch the device either on or off.

## Usage

This OTP app features it's own optional name-based supervision of Vera clients. You may start named clients with the `veraerl:start_child/2` method. You may also just start your own `vera_client` and handle the supervision yourself.

By leveraging the `shellbeam` capability of [`magicbeam`](http://github.com/otakup0pe/magicbeam) a simple command line client is available as an escript.