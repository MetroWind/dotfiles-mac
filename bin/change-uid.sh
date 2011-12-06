#!/bin/zsh
#
# ======================================================================
# Change UID in Mac OS X
# ======================================================================

if [ $# != 2 ]; then
    print "Usage: $0 USERNAME NEW_UID"
    exit 1
fi

UName=$1

getUID ()
{
    dscl . -read /Users/$UName UniqueID | sed -E 's/UniqueID: ([0-9]+)/\1/g'
}

NewUID=$2
OldUID=$(getUID)

Volumes=("Shared" "Home")

print -n "Changing ${UName}'s UID from ${OldUID} to ${NewUID}.  Proceed? (y/n)"
read YN
if [ $YN != 'y' ]; then
    exit 255
fi

print "Changing UID..."
dscl . -change /Users/$UName UniqueID ${OldUID} ${NewUID}

# if [ $(getUID) != ${NewUID} ]; then
#     print "Failed to change UID.  Exiting..."
#     exit 2
# fi

print "Changing mode for home dir..."
find /Users/$UName -user ${OldUID} -exec chown ${NewUID} '{}' \;
print "Changing mode for /Library..."
find /Library -user ${OldUID} -exec chown ${NewUID} '{}' \;
print "Changing mode for /Applications..."
find /Applications -user ${OldUID} -exec chown ${NewUID} '{}' \;
print "Changing mode for /usr..."
find /usr -user ${OldUID} -exec chown ${NewUID} '{}' \;
print "Changing mode for /private/var..."
find /private/var -user ${OldUID} -exec chown ${NewUID} '{}' \;

for Vol in ${Volumes}; do
    print "Changing mode for volume \"$Vol\"..."
    find /Volumes/${Vol} -user ${OldUID} -exec chown ${NewUID} '{}' \;
done

if [ -e /.Trashes/${OldUID} ]; then
    print "Changing name for /.Trashes..."
    mv /.Trashes/${OldUID} /.Trashes/${NewUID}
fi

for Vol in ${Volumes}; do
    if [ -e /Volumes/${Vol}/.Trashes/${OldUID} ]; then
        print "Changing name for /Volumes/${Vol}/.Trashes..."
        mv /Volumes/${Vol}/.Trashes/${OldUID} /Volumes/${Vol}/.Trashes/${NewUID}
    fi
done

print "Changing name for misc. files..."
if [ -e /Library/Caches/com.apple.ImageCaptureExtension2.ICADeviceDatabase.${OldUID} ]; then
    mv /Library/Caches/com.apple.ImageCaptureExtension2.ICADeviceDatabase.${OldUID} \
        /Library/Caches/com.apple.ImageCaptureExtension2.ICADeviceDatabase.${NewUID}
fi

if [ -e /Library/Caches/com.apple.ImageCaptureNotifications.DeviceDiscoveryDatabase.${OldUID} ]; then
    mv /Library/Caches/com.apple.ImageCaptureNotifications.DeviceDiscoveryDatabase.${OldUID} \
        /Library/Caches/com.apple.ImageCaptureNotifications.DeviceDiscoveryDatabase.${NewUID}
fi

if [ -e /private/var/db/launchd.db/com.apple.launchd.peruser.${OldUID} ]; then
    mv /private/var/db/launchd.db/com.apple.launchd.peruser.${OldUID} \
        /private/var/db/launchd.db/com.apple.launchd.peruser.${NewUID}
fi

if [ -e /.DocumentRevisions-V100/PerUID/${OldUID} ]; then
    mv /.DocumentRevisions-V100/PerUID/${OldUID} \
        /.DocumentRevisions-V100/PerUID/${NewUID}
fi

if [ -e /Library/DropboxHelperTools/Dropbox_u${OldUID} ]; then
    mv /Library/DropboxHelperTools/Dropbox_u${OldUID} \
        /Library/DropboxHelperTools/Dropbox_u${NewUID}
fi

for File in $(find /private/var/folders -name "*${OldUID}"); do
    mv ${File} $(${File} | sed "s/${OldUID}\$/${NewUID}/g")
done

print "All done."
