#!/bin/bash
if [ -e .username ]; then
    username=$(cat .username)
else
    echo Enter username:
    read username
fi
echo $username > .username

if [ -e .password ]; then
    password=$(cat .password)
else
    echo Enter password:
    read password
fi
echo $password > .password

if [ -e .location ]; then
    location=$(cat .location)
else
    echo Enter location:
    read location
fi
echo $location > .location

#those packages are required: smbfs smbclient
#mount POWERVAULT/USERS/FABRE
sudo mount -t smbfs -o username=$username,password=$password $location/users/fabre ~/powervault/fabre
#mount POWERVAULT/PHOTONICLATTICE
sudo mount -t smbfs -o username=$username,password=$password $location/photoniclattice ~/powervault/photoniclattice
#mount POWERVAULT/REPOSITORIES
sudo mount -t smbfs -o username=$username,password=$password $location/repositories ~/powervault/repositories