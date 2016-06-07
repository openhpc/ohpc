#!/usr/bin/perl
use strict;
use warnings;

my $all_yaml;



run();

############################################################################
    # run()
    #
    # generate yaml and write it to a file
    #
    #   - args:     none
    #
    #   - returns:  none
    #

  sub run{
    get_sys_info();
    my $dir = "../";
    opendir(DIR, $dir) or die "couldn't open $dir: $!\n";
    my @files = readdir(DIR);
    closedir(DIR);
    foreach my $file (@files){
      if (-T "$dir/$file"){
        get_result_info("$dir/$file");
      }
    }
    print $all_yaml;
    my $filePath = $dir . "upload/" . "mantevo_results.yaml";
    open(my $out, ">", $filePath  ) or die "Can't open $filePath: $!";
    print  $out $all_yaml; 
  } # run

############################################################################
    # get_sys_info()
    #
    # gather system information into yaml format and append it to the "all_yaml" string
    #
    #   - args:     none
    #
    #   - returns:  none
    #

    sub get_sys_info {
      my $hostName = "";          my $dnsName = "";           my $ipAddress = "";     
      my $operatingSystem = "";   my $kernelName = "";        my $kernelRelease = "";
      my $kernelVersion = "";     my $processor = "";         my $machineHardware = "";
      my $hardwarePlatform = "";  my $badCmd = 0;          	my $outString = "";
      my $time = "";
      $badCmd = system('hostname -s > /dev/null 2>&1');
      if (!$badCmd) {
        chomp($hostName=`hostname -s`);
      } else {
        $hostName=getOptionalOutput("uname -n");
      }
      $dnsName=getOptionalOutput("hostname -d");
      $ipAddress=getOptionalOutput("hostname -i");
      $operatingSystem=getOptionalOutput("uname -o");
      $kernelName=getOptionalOutput("uname -s");
      $kernelRelease=getOptionalOutput("uname -r");
      $kernelVersion=getOptionalOutput("uname -v");
      $processor=getOptionalOutput("uname -p");
      $machineHardware=getOptionalOutput("uname -m");
      $hardwarePlatform=getOptionalOutput("uname -i");
      $time = time();
      $all_yaml = "---
description: $time
hostName: $hostName
dnsName: $dnsName
ipAddress: $ipAddress
operatingSystem: $operatingSystem
kernelName: $kernelName
kernelRelease: $kernelRelease
kernelVersion: $kernelVersion
processor: $processor
machineHardware: $machineHardware
hardwarePlatform: $hardwarePlatform

";
    } # get_sys_info()

############################################################################
    # get_result_info()
    #
    # appends the yaml from a result file to the "all_yaml" string
    #
    #   - args:     the shell command
    #
    #   - returns:  the optional output string
    #

    sub get_result_info {
      my $current_file = $_[0];
      local $/=undef;
      open FILE, $current_file or die "Couldn't open $current_file: $!";
      my $yaml_string = <FILE>;
      close FILE;
      $all_yaml = "$all_yaml--SEPARATOR--\n\n$yaml_string\n";
    } # get_result_info()

############################################################################
    # getOptionalOutput()
    #
    # Runs a system command and gets its output if it succeeds then returns
    # its output.  The program suppress output to stderr of the program does
    # not run correctly.
    #
    #   - args:     the shell command
    #
    #   - returns:  the optional output string
    #

    sub getOptionalOutput {
      my $cmnd_in = shift;
      my $cmnd = "$cmnd_in > /dev/null 2>&1";
      my $returnVal = system($cmnd);
      my $outputStr="";
      chomp($outputStr = `$cmnd_in`) if($returnVal == 0);
      return $outputStr;
    } # getOptionalOutput()