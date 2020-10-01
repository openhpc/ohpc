if [ -z "$__Init_Default_Modules" ]; then
	export __Init_Default_Modules=1;
	module --initial_load --no_redirect restore
else
	module refresh
fi
