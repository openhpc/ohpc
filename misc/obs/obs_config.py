#!/usr/bin/env python3
#
# utility to create parent/child packages in OBS for a new OHPC
# version based on configuration specified in an ini style config
# file.
# --
import configparser
import logging
import argparse
import sys
import os
import ast
import tempfile
import inspect
import re
from semver import VersionInfo
import subprocess
from xml.etree import ElementTree
import coloredlogs

#---
# global config settings
obsurl="https://build.openhpc.community"
version_in_progress="1.3.7"
configFile="config"

#---
# Simple error wrapper to include exit
def ERROR(output):
    logging.error(output)
    sys.exit()

#---
# Main worker class to read config setup from file and interact with OBS
class ohpc_obs_tool(object):
    def __init__(self):
        coloredlogs.install(level='INFO',fmt="%(message)s")
#        logging.basicConfig(format="%(message)s",level=logging.INFO,stream=sys.stdout)
#        logger = logging.getLogger()
#        logger.setLevel("INFO")
        logging.info("\nVersion in Progress = %s" % version_in_progress)

        self.vip            = version_in_progress

        self.buildConfig    = None
        self.parentCompiler = None
        self.parentMPI      = None
        self.dryRun         = True
        self.buildsToCancel = []

        # parse version to derive obs-specific version info
        vparse = VersionInfo.parse(self.vip)
        self.branchVer = str(vparse.major) + '.' + str(vparse.minor)
        self.microVer  = str(vparse.patch)

        logging.info("--> Branch version  = %s" % self.branchVer)
        logging.info("--> Micro release   = %s" % self.microVer)

        self.obsProject="OpenHPC:" + self.branchVer + ":Update" + self.microVer + ":Factory"
        logging.info("--> OBS project     = %s" % self.obsProject)

    def checkForDisabledComponents(self,components):
        activeComponents = []
        for item in components:
            if item[0] == '!':
                logging.warn("--> Skipping disabled component %s" % item)
            else:
                activeComponents.append(item)
        return activeComponents


    def parseConfig(self,configFile=None):
        assert(configFile is not None)
        logging.info("\nReading config information from file = %s" % configFile)
        if os.path.isfile(configFile):
            self.buildConfig = configparser.ConfigParser(inline_comment_prefixes='#',interpolation=configparser.ExtendedInterpolation())
            try:
                self.buildConfig.read(configFile)
            except configparser.DuplicateSectionError:
                ERROR("\nERROR: Duplicate section detected in configfile: %s" % configFile)
            except:
                ERROR("ERROR; Unable to parse runtime config file: %s" % configFile)

            logging.info("--> file parsing ok")

            # read global settings for this version in progress
            vip = version_in_progress

            try:
                self.dryRun            = self.buildConfig.getboolean('global','dry_run',fallback=True)
                self.serviceFile       = self.buildConfig.get('global','service_template')
                self.linkFile_compiler = self.buildConfig.get('global','link_compiler_template')
                self.linkFile_mpi      = self.buildConfig.get('global','link_mpi_template')
                self.compilerFamilies  = ast.literal_eval(self.buildConfig.get(vip,'compiler_families'))
                self.MPIFamilies       = ast.literal_eval(self.buildConfig.get(vip,'mpi_families'))


            except:
                ERROR("Unable to parse global settings for %s" % vip)

            assert(len(self.compilerFamilies) > 0)
            assert(len(self.MPIFamilies) > 0)

            self.parentCompiler = self.compilerFamilies[0]
            self.parentMPI      = self.MPIFamilies[0]

            logging.info("--> (global) dry run              = %s" % self.dryRun)
            logging.info("--> (global) service template     = %s" % self.serviceFile)
            logging.info("--> (global) link template (comp) = %s" % self.linkFile_compiler)
            logging.info("--> (global) link template (mpi)  = %s" % self.linkFile_mpi)
            logging.info("\nCompiler families (%s):" % self.vip)
            
            for family in self.compilerFamilies:
                output = "--> %s" % family
                if(family is self.parentCompiler):
                    output += " (parent)"
                logging.info(output)

            logging.info("\nMPI families (%s):" % (self.vip))
            for family in self.MPIFamilies:
                output = "--> %s" % family
                if(family is self.parentMPI):
                    output += " (parent)"
                logging.info(output)
#            logging.info("-" * 40)
            logging.info("")

            # parse skip patterns
            self.NoBuildPatterns ={}

            if self.buildConfig.has_option(self.vip,'skip_aarch'):
                self.NoBuildPatterns['aarch64'] = ast.literal_eval(self.buildConfig.get(vip,'skip_aarch'))
            if self.buildConfig.has_option(self.vip,'skip_x86'):
                self.NoBuildPatterns['x86_64'] = ast.literal_eval(self.buildConfig.get(vip,'skip_x86'))

            logging.info("Architecture skip patterns:")
            for pattern in self.NoBuildPatterns:
                logging.info("--> arch = %6s, pattern(s) to skip = %s" % (pattern,self.NoBuildPatterns[pattern]))

            # cache group definition(s)
            self.groups={}

            try:
                groups=self.buildConfig.options("groups")
                assert(len(groups) > 0)
            except:
                ERROR("Unable to parse [group] names")

            logging.info("--> (global) %i package groups defined:" % len(groups))

            # read in components assigned to each group
            for group in groups:
                try:
                    components=ast.literal_eval(self.buildConfig.get("groups",group))

                except:
                    ERROR("Unable to parse component groups")
                
                self.groups[group] = components

            for group in groups:
                logging.info("    --> %-20s: %2i components included" % (group,len(self.groups[group])))
                for name in self.groups[group]:
                    logging.debug("        ... %s" % name)

            logging.info("")

        else:
            ERROR("--> unable to access input file")

    #---
    # query components defined in config file for version in progress
    # Return: list of component names
    def query_components(self,version="unknown"):
        sections = self.buildConfig.sections()

        ver_delim = self.vip + '/'      # we expect a slash in config file (e.g. 1.3.7/cmake)

        components = {}

        if self.buildConfig.has_option(self.vip,'standalone'):
            components['standalone'] = ast.literal_eval(self.buildConfig.get(self.vip,"standalone"))
            logging.info("Parsed components:")
            logging.info("--> [standalone]: %s" % components['standalone'])
        
        if self.buildConfig.has_option(self.vip,'compiler_dependent'):
            components['comp_dep'] = ast.literal_eval(self.buildConfig.get(self.vip,"compiler_dependent"))
            logging.info("--> [  comp_dep]: %s" % components['comp_dep'])

        if self.buildConfig.has_option(self.vip,'mpi_dependent'):
            components['mpi_dep'] = ast.literal_eval(self.buildConfig.get(self.vip,"mpi_dependent"))
            logging.info("--> [   mpi_dep]: %s" % components['mpi_dep'])

            components['mpi_dep'] = self.checkForDisabledComponents(components['mpi_dep'])

        numComponents = len(components['standalone']) + len(components['comp_dep']) + len(components['mpi_dep'])
        logging.info("# of requested components = %i\n" % numComponents)
        return(components)

    #---
    # query all packages currently defined for given version in obs
    # Return: dict of defined packages
    def queryOBSPackages(self):
        base="OpenHPC"
        logging.info("[queryOBSPackages]: checking for packages currently defined in OBS (%s)" % self.vip)

        command = ["osc","api","-X","GET","/source/" + self.obsProject]
        try:
            s = subprocess.check_output(command)
        except:
            ERROR("Unable to queryPackages from obs")

        results = ElementTree.fromstring(s)
        node    = results.find('./directory')

        packages = {}

        for value in results.iter('entry'):
            packages[value.get('name')] = 1

        logging.info("[queryOBSPackages]: %i packages defined" % len(packages))
        logging.debug(packages)
        return(packages)

    #---
    # check if package is standalone (ie, not compiler or MPI dependent)
    def isStandalone(self,package):
        fname = inspect.stack()[0][3]
        compiler_dep = self.buildConfig.getboolean(self.vip + '/' + package,'compiler_dep',fallback=False)
        mpi_dep      = self.buildConfig.getboolean(self.vip + '/' + package,'mpi_dep',     fallback=False)

        logging.debug("\n[%s] - %s: compiler_dep = %s" % (fname,package,compiler_dep))
        logging.debug("[%s] - %s: mpi_dep      = %s" % (fname,package,mpi_dep))

        if compiler_dep or mpi_dep:
            return(False)
        else:
            return(True)

    #---
    # check if package is compiler dependent (ie, depends on compiler family, but not MPI)
    def isCompilerDep(self,package):
        fname = inspect.stack()[0][3]
        compiler_dep = self.buildConfig.getboolean(self.vip + '/' + package,'compiler_dep',fallback=False)
        mpi_dep      = self.buildConfig.getboolean(self.vip + '/' + package,'mpi_dep',     fallback=False)

        logging.debug("\n[%s] - %s: compiler_dep = %s" % (fname,package,compiler_dep))
        logging.debug("[%s] - %s: mpi_dep      = %s" % (fname,package,mpi_dep))

        if compiler_dep and not mpi_dep:
            return(True)
        else:
            return(False)

    #---
    # check if package is MPI dependent (implies compiler toolchain dependency)
    def isMPIDep(self,package):
        fname = inspect.stack()[0][3]
        mpi_dep      = self.buildConfig.getboolean(self.vip + '/' + package,'mpi_dep',     fallback=False)

        logging.debug("\n[%s] - %s: mpi_dep      = %s" % (fname,package,mpi_dep))

        if mpi_dep:
            return(True)
        else:
            return(False)

    #---
    # check which group a package belongs to
    # return: name of group (str)
    def checkPackageGroup(self,package):
        fname = inspect.stack()[0][3]
        found = False
        for group in self.groups:
            if package in self.groups[group]:
                logging.debug("[%s] %s belongs to group %s" % (fname,package,group))
                return(group)
        if not found:
            ERROR("package %s not assocated with any groups, please check config" % package)

    #---
    # update dryrun options
    def overrideDryRun(self):
        self.dryRun = False
        return

    #---
    # return parent compiler
    def getParentCompiler(self):
        return self.parentCompiler


    #---
    # return parent cMPI
    def getParentMPI(self):
        return self.parentMPI

    #---
    # check package against skip build patterns 
    def disableBuild(self,package,arch):
        fname = inspect.stack()[0][3]
        if arch not in self.NoBuildPatterns:
            return False
        
        for pattern in self.NoBuildPatterns[arch]:
            if re.search(pattern,package):
                logging.debug("[%s]: %s found in package name (%s)" % (fname,pattern,package))
                return True
        return False
            
            
    #--- query compiler family builds for given package. Default to
    #--  global settings unless overridden by package specific settings
    def queryCompilers(self,package,noOverride=False):
        fname = inspect.stack()[0][3]
        compiler_families = self.compilerFamilies

        if(noOverride):
            return(compiler_families)
        
        # check if any override options exist for this package
        if self.buildConfig.has_option(self.vip,package + "_compiler"):
            compiler_families = ast.literal_eval(self.buildConfig.get(self.vip,package + "_compiler"))

        logging.debug("[%s]: %s" % (fname,compiler_families))
        return(compiler_families)

    #--- query MPI family builds for given package. Default to
    #--  global settings unless overridden by package specific settings
    def queryMPIFamilies(self,package):
        fname = inspect.stack()[0][3]
        mpi_families = self.MPIFamilies
        
        # check if any override options
        if self.buildConfig.has_option(self.vip + "/" + package,"mpi_families"):
            mpi_families = ast.literal_eval(self.buildConfig.get(self.vip + "/" + package,"mpi_families"))
            logging.info("\n--> override of default mpi families requested for package = %s" % package)
            logging.info("--> families %s\n" % mpi_families)

        logging.debug("[%s]: %s" % (fname,mpi_families))
        return(mpi_families)

    #---
    # add specified package to OBS
    def addPackage(self,package,parent=True,isCompilerDep=False,isMPIDep=False,compiler=None,mpi=None,parentName=None,gitName=None):
        fname = inspect.stack()[0][3]

        # verify we have template _service file
        if os.path.isfile(self.serviceFile):
            with open(self.serviceFile,'r') as filehandle:
                contents = filehandle.read()
            filehandle.close()
        else:
            ERROR("Unable to read _service file template" % self.serviceFile)

        pad = 15

        # verify we have a group definition for the parent package
        if(parent):
            if gitName is not None:
                group = self.checkPackageGroup(gitName)
            else:
                group = self.checkPackageGroup(package)
            logging.debug("[%s]: group assigned = %s" % (fname,group))

        # Step 1: create _meta file for obs package (this defines new obs package)
        fp = tempfile.NamedTemporaryFile(delete=True,mode='w+t')
        fp.writelines("<package name = \"%s\" project=\"%s\">\n" % (package,self.obsProject))
        fp.writelines("<title/>\n")
        fp.writelines("<description/>")
        fp.writelines("<build>\n")

        # check skip pattern to define build architectures
        if self.disableBuild(package,'aarch64'):
            logging.warn(" " * pad + "--> disabling aarch64 build per pattern match request")
            fp.writelines("<disable arch=\"aarch64\"/>\n")
        else:
            fp.writelines("<enable arch=\"aarch64\"/>\n")

        if self.disableBuild(package,'x86_64'):
            logging.warn(" " * pad + "--> disabling x86_64 build per pattern match request")
            fp.writelines("<disable arch=\"x86_64\"/>\n")
        else:
            fp.writelines("<enable arch=\"x86_64\"/>\n")

        fp.writelines("</build>\n")
        fp.writelines("</package>\n")
        fp.flush()
        
        logging.debug("[%s]: new package _metadata written to %s" % (fname,fp.name))
        command = ["osc","api","-f",fp.name,"-X","PUT","/source/" + self.obsProject + "/" + package + "/_meta"] 


        if self.dryRun:
            logging.error(" " * pad + "--> (dryrun) requesting addition of package: %s" % package)
            
        logging.debug("[%s]: (command) %s" % (fname,command))

        if not self.dryRun:
            try:
                s = subprocess.check_output(command)
            except:
                ERROR("\nUnable to add new package (%s) to OBS" % package)

        # add marker file indicating this is a new OBS addition ready to be rebuilt (nothing in file, simply a marker)
        if (True):
            fp = tempfile.NamedTemporaryFile(delete=False,mode='w+t')
            fp.flush()

            markerFile = "_obs_config_ready_for_build"
            command = ["osc","api","-f",fp.name,"-X","PUT","/source/" + self.obsProject + "/" + package + "/" + markerFile]  
            if self.dryRun:
                logging.debug(" " * pad + "--> (dryrun) requesting addition of %s file for package: %s" % (markerFile,package))
                
            logging.debug("[%s]: (command) %s" % (fname,command))

            if not self.dryRun:
                try:
                    s = subprocess.check_output(command)
                except:
                    ERROR("\nUnable to add marker file for package (%s) to OBS" % package)
        

        if(parent):   # Step 2a: add _service file for parent package

            # obs needs escape for hyphens in _service file
            group = group.replace('-','[-]')

            # create package specific _service file

            pname = package
            if gitName is not None:
                pname = gitName
            contents = contents.replace('!GROUP!',  group)
            contents = contents.replace('!PACKAGE!',pname)
            contents = contents.replace('!VERSION!',self.vip)

            fp_serv = tempfile.NamedTemporaryFile(delete=True,mode='w')
            fp_serv.write(contents)
            fp_serv.flush()
            logging.debug("--> _service file written to %s" % fp_serv.name)

            command = ["osc","api","-f",fp_serv.name,"-X","PUT","/source/" + self.obsProject + "/" + package + "/_service"]  

            if self.dryRun:
                logging.error(" " * pad + "--> (dryrun) adding _service file for package: %s" % package)
            
            logging.debug("[%s]: (command) %s" % (fname,command))

            if not self.dryRun:
                try:
                    s = subprocess.check_output(command)
                except:
                    ERROR("\nUnable to add _service file for package (%s) to OBS" % package)

        else: # Step2b: add _link file for child package
            
            if isCompilerDep:
                linkFile = self.linkFile_compiler
                assert(compiler is not None)
            elif isMPIDep:
                linkFile = self.linkFile_mpi
            
            assert(parentName is not None)
                
            # verify we have template _link file template
            if os.path.isfile(linkFile):
                with open(linkFile,'r') as filehandle:
                    contents = filehandle.read()
                    filehandle.close()
            else:
                ERROR("Unable to read _link file template" % linkFile)

            # create package specific _link file

            contents = contents.replace('!PACKAGE!',parentName)
            contents = contents.replace('!COMPILER!',compiler)
            contents = contents.replace('!PROJECT!',self.obsProject)
            if isMPIDep:
                contents = contents.replace('!MPI!',mpi)


            fp_link = tempfile.NamedTemporaryFile(delete=True,mode='w')
            fp_link.write(contents)
            fp_link.flush()
            logging.debug("--> _link file written to %s" % fp_link.name)

            command = ["osc","api","-f",fp_link.name,"-X","PUT","/source/" + self.obsProject + "/" + package + "/_link"]  

            if self.dryRun:
                logging.error(" " * pad + "--> (dryrun) adding _link file for package: %s (parent=%s)" % (package,parentName))
            
            logging.debug("[%s]: (command) %s" % (fname,command))

            if not self.dryRun:
                try:
                    s = subprocess.check_output(command)
                except:
                    ERROR("\nUnable to add _link file for package (%s) to OBS" % package)

        # Step 3 - register package to lock build once it kicks off 
        self.buildsToCancel.append(package)

    def cancelNewBuilds(self):
        fname = inspect.stack()[0][3]
        numBuilds = len(self.buildsToCancel)

        if(numBuilds == 0):
            logging.info("\nNo new builds created.")
            return
        else:
            logging.info("\n%i new build(s) need to be locked:" % numBuilds)
            logging.info("--> will lock for now and GitHub trigger will unlock on first commit")

        for package in self.buildsToCancel:
            command = ["osc","lock", self.obsProject,package]

            if self.dryRun:
                logging.info("--> (dryrun) requesting lock for package: %s" % package)

            logging.debug("[%s]: (command) %s" % (fname,command))

            if not self.dryRun:
                try:
                    s = subprocess.check_output(command)
                except:
                    ERROR("\nUnable to add _link file for package (%s) to OBS" % package)



# -- 
# top-level

def main():        

    # parse command-line args
    parser = argparse.ArgumentParser()
    parser.add_argument("--configFile",help="filename with package definition options (default = config)",type=str)
    parser.add_argument("--no-dryrun",dest='dryrun',help="flag to disable dryrun mode and execute obs commands",action="store_false")
    parser.set_defaults(dryrun=True)
    args = parser.parse_args()

    # main worker bee class
    obs = ohpc_obs_tool()

    # read config file and parse component packages desiredfor current version
    obs.parseConfig(configFile=configFile)
    components = obs.query_components()

    # override dryrun option is requested
    if not args.dryrun:
        logging.info("--no-dryrun command line arg requested: will execute commands\n")
        obs.overrideDryRun()

    # query components defined in existing OBS project
    obsPackages = obs.queryOBSPackages()

    # Check if desired package(s) are present in OBS and add them if
    # not. Different logic applies to (1) standalone packages,
    # (2) packages with a compiler dependency, and (3) packages with an
    # MPI dependency

    logging.info("")

    # (1) standalone packages
    for package in components['standalone']:
        ptype="standalone"
        if package in obsPackages:
            logging.info("%27s (%13s): present in OBS" % (package,ptype))
        else:
            logging.info("%27s (%13s): *not* present in OBS, need to add" % (package,ptype))
            obs.addPackage(package,parent=True)

    # (2) compiler dependent packages
    for package in components['comp_dep']:
        ptype     = "compiler dep"
        parent    = package + '-' + obs.getParentCompiler()

        compilers    = obs.queryCompilers(package)
        Defcompilers = obs.queryCompilers(package,noOverride=True)

        if compilers != Defcompilers:
            pad = 15
            logging.warn(" " * pad + "--> override of default compiler families requested for %s" % package)
            logging.warn(" " * pad + "--> families =  %s" % compilers)

        # check on parent first (it must exist before any children are linked)
        if parent in obsPackages:
            logging.info("%27s (%13s): present in OBS" % (parent,ptype))
        else:
            logging.info("%27s (%13s): *not* present in OBS, need to add" % (parent,ptype))
            obs.addPackage(parent,parent=True,isCompilerDep=True,gitName=package)

        # now, check on children
        for compiler in compilers:
            # verify compiler is known (as user could override with unknown compiler family)
            if compiler not in Defcompilers:
                ERROR("requested compiler %s is not one of known compiler families; double check config file" % compiler)
                   
            if compiler == obs.getParentCompiler():
                logging.debug("...skipping parent compiler...")
                continue

            child = package + '-' + compiler
            logging.debug(" " * 22 + "checking on child compiler dependent package: %s" % child)
            if child in obsPackages:
                logging.info("%27s (%13s): present in OBS" % (child,ptype))
            else:
                logging.info("%27s (%13s): *not* present in OBS, need to add" % (child,ptype))
                obs.addPackage(child,parent=False,isCompilerDep=True,compiler=compiler,parentName=parent)

    # (3) MPI dependent packages
    for package in components['mpi_dep']:
        ptype     = "MPI dep"
        parent    = package + '-' + obs.getParentCompiler() + '-' + obs.getParentMPI()
        compilers = obs.queryCompilers(package)
        mpiFams   = obs.queryMPIFamilies(package)
        
        # check on parent first (it must exist before any children are linked)
        if parent in obsPackages:
            logging.info("%27s (%13s): present in OBS" % (parent,ptype))
        else:
            logging.info("%27s (%13s): *not* present in OBS, need to add" % (parent,ptype))
            obs.addPackage(parent,parent=True,isMPIDep=True,gitName=package)
    
        # now, check on children
        for compiler in compilers:
            for mpi in mpiFams:
                child = package + '-' + compiler + '-' + mpi;
                if (child == parent):
                    logging.debug("...skipping parent package %s" % child)
                    continue
                
                if child in obsPackages:
                    logging.info("%27s (%13s): present in OBS" % (child,ptype))
                else:
                    logging.info("%27s (%13s): *not* present in OBS, need to add" % (child,ptype))
                    obs.addPackage(child,parent=False,isMPIDep=True,compiler=compiler,mpi=mpi,parentName=parent)
    
    obs.cancelNewBuilds()
    

if __name__ == '__main__':
    main()


    



