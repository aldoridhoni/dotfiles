# Defined in /home/aldo/.config/fish/conf.d/mac.fish @ line 1
function is_mac
	test -e '/System/Library/CoreServices/SystemVersion.plist'
end
