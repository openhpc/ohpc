$mypid = fork();
if (! $mypid) {
	system('mpirun arrays_read &>arrays_read.out');
} elsif (undef $mypid) {
	die 'fork failed';
} else {
	system('mpirun arrays_write &>arrays_write.out');
}
