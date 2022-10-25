#!/usr/bin/env lua
--[[
===============================================================================
This script is part of the OpenHPC project.

It may have been modified from the default version supplied by the underlying
release package (if available) in order to apply patches, perform customized
build/install configurations, and supply additional files to support
desired integration conventions.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License
===============================================================================
]]

local function description()
    return [[
ONEAPI-MODGEN
    OpenHPC - Lmod Module Generator for Intel(R) oneAPI Toolkits
    
    Creates and manages OpenHPC Lmod module creation for Intel(R) oneAPI
    toolkits and legacy software tools. The default installation path
    /opt/intel/oneapi is used to locate installed versions. Other
    installations can be added manually using the root path for that
    installation.
    ]]
end

require "io"
require "os"
require "lfs" -- lua-filesystem RPM required

-- Global constants
C = {}
C["OHPCROOT"] = (os.getenv("OHPCROOT") or "/opt/ohpc")
C["MODPATH"] = C.OHPCROOT .. "/pub/modulefiles"
C["DEPPATH"] = C.OHPCROOT .. "/pub/moduledeps"
C["ONEAPIMODS"] = C.DEPPATH .. "/oneapi"
C["RTMODS"] = C.MODPATH .. "/intel-rt"
C["MANIFESTPATH"] = C.OHPCROOT .. "/admin/intel"
C["ONEAPIPATH"] = "/opt/intel/oneapi"
C["MODSCRIPT"] = "/modulefiles-setup.sh"
C["SCRIPTOPTS"] = " --ignore-latest --force --output-dir=" ..
    C.ONEAPIMODS .. "/ "
C["GNU"] = "12"
-- Lock the constant table
C = setmetatable({}, {
    __index = C,
    __newindex = function(t, k, v)
        error("Attempt to alter constant value", 2)
    end,
    __metatable = false
})

-- Command line commands
Commands = setmetatable({}, {
    __index = {
        new = function(t, n, h, a, c, i)
            t[n] = {
                help = h, -- Help text
                argc = a, -- No. of required arguments (int)
                call = c, -- Handler function
                ishidden = i -- Hide command (bool)
            }
            return t
        end
    }
})

--[[
Modules - Compilers, MPI, etc.
    [<devel tool name>] = {
        help = <help text>
        path = <LMOD module path>]
        name = <tool name>,
        find = <search text for full version only>,
        rt = <search text for runtimes>
        }
]]
ModTypes = {
    ["compiler"] = {
        help = "Intel(R) C and Fortran Compilers",
        path = C.MODPATH .. "/intel",
        name = "compiler",
        find = "/bin/icx",
        rt = "/modulefiles/compiler"
    },
    ["gnu-mkl"] = {
        help = "Intel(R) Math Kernel Library, for gcc",
        path = C.DEPPATH .. "/gnu/mkl",
        name = "mkl",
        find = "/include/mkl.h",
        rt = "/modulefiles/mkl"
    },
    ["mpi"] = {
        help = "Intel(R) MPI Compiler & Libraries",
        path = C.DEPPATH .. "/intel/impi",
        name = "mpi",
        find = "/bin/mpiicc",
        rt = "/modulefiles/mpi"
    },
    ["gnu-mpi"] = {
        help = "Intel(R) MPI Compiler & Libraries, for gcc",
        path = C.DEPPATH .. "/gnu/impi",
        name = "mpi",
        find = "/bin/mpigcc",
        rt = "/modulefiles/mpi"
    },
    ["gnu" .. C.GNU .. "-mpi"] = {
        help = "Intel(R) MPI Compiler & Libraries, for gcc" .. C.GNU,
        path = C.DEPPATH .. "/gnu" .. C.GNU .. "/impi",
        name = "mpi",
        find = "/bin/mpigcc",
        rt = "/modulefiles/mpi"
    }
}

-- General format for storing manifest in memory
Metamanifest = {
    __index = {
        new = function(t, v, f, h, i)
            -- v = version
            t[v] = {
                file = f, -- Full path and filename
                md5 = h, -- MD5 hash
                isdiff = i -- Modified file (bool)
            }
            return t
        end,
        -- Find and return the latest version in module table
        latest = function(t)
            local vtab = {}
            for k, _ in pairs(t) do
                if k ~= "oneapi" then
                    table.insert(vtab, k)
                end
            end
            table.sort(vtab)
            return vtab[#vtab]
        end
    }
}

------------------------------
-- Generic Global Functions --
------------------------------

-- Remove leading directories from file path
function BaseName(path)
    return tostring(path:match(".*/([^/]*)$"))
end

function DirName(path)
    return tostring(path:match("(.*)/[^/]+$"))
end

-- Concatenate all arguments into a path
function PathCat(...)
    local path = ""
    local d = { ... }
    for i, v in ipairs(d) do
        if i == 1 then
            path = tostring(v)
        else
            path = path .. "/" .. tostring(v)
        end
    end
    path = path:gsub("/+", "/")
    return path
end

-- Execute a shell command string and return output
function RunOSCommand(cmd)
    local handle = assert(io.popen(cmd, 'r'),
        "Process open error: '" .. cmd .. "'")
    handle:flush()
    local cmdoutput = assert(handle:read('*a'),
        "Process read eoor: '" .. cmd .. "'")
    -- Command returns STDOUT (minus newline) and if no error
    return cmdoutput:gsub("\n+$", ""), not handle:close()
end

-- Convert multi-line string to array
function StringtoArray(str)
    local splitstring = {}
    for l in str:gmatch("[^\n]*") do
        table.insert(splitstring, l)
    end
    return splitstring
end

-- Read the contents of a file into an array
function ReadFile(filename)
    local fh, err = io.open(filename, "r")
    if fh then
        local data, err = string.gsub(fh:read("a"), "\n$", "")
        fh:close()
        if data then
            return StringtoArray(data), nil
        else
            return nil, "Cannot read file " .. filename ..
                "\n   " .. tostring(err)
        end
    else
        return nil, "Cannot open file " .. filename ..
            "\n   " .. tostring(err)
    end
end

-- Move a file to new name and add extension
function BackupFIle(filename, ext)
    local newext = ext or ".backup"
    local success, err = os.rename(filename, filename .. newext)
    if not success then
        return nil, "Cannot rename " .. filename .. "\n   " .. tostring(err)
    else
        return filename .. newext
    end
end

-- Write a string to a file and close it
function WriteFile(filename, data)
    local fh, err = io.open(filename, "w")
    if fh then
        local success, err = fh:write(data)
        fh:close()
        if not success then
            return nil, "Cannot write to file " .. filename ..
                "   \n" .. tostring(err)
        else
            return true, ""
        end
    else
        return nil, "Cannot open file " .. filename .. "   \n" .. tostring(err)
    end
end

---------------------
-- Local Functions --
---------------------

-- Print in consistent format and to STDERR as needed
local function printformat(txt, mode)
    if mode == "warning" then
        if not assert(io.stderr:write("WARNING: " .. txt .. "\n"),
            "Cannot write to STDERR") then
            os.exit(1)
        else
            return true
        end
    end
    if mode == "debug" then
        if not assert(io.stderr:write("***\nDEBUG: " .. txt .. "\n"),
            "Cannot write to STDERR") then
            os.exit(1)
        else
            return true
        end
    end
    if mode == "error" then
        assert(io.stderr:write("\nERROR: " .. txt .. "\n\n"),
            "Cannot write to STDERR")
        os.exit(1)
    end
    if mode == "header" then
        print("\n=== " .. txt .. " ===")
        return true
    end
    if mode == "step" then
        print("  - " .. txt)
        return true
    end
    if mode == "substep" then
        print("       " .. txt)
        return true
    end
    if mode == "info" or not mode then
        print(txt)
        return true
    end
    error("Mode " .. mode .. " is not defined.", 2)
    return nil
end

-- Check that a directory exists
-- If required, either create or terminate
local function dir_exists(path, isrequired)
    local mode = lfs.attributes(path, "mode")
    if not mode then
        if isrequired then
            local _, iserror = RunOSCommand("mkdir -p " .. path)
            if iserror then
                printformat("Cannot create directory" .. path, "error")
            end
        else
            return false
        end
    else -- mode is set
        if mode ~= "directory" then
            if isrequired then
                printformat(path .. " is not a directory", "error")
            else
                printformat(path .. " is not a directory", "warning")
                return false
            end
        else
            return true
        end
    end
end

-- Return manifest path
local function manifest_path(modtype)
    return PathCat(C.MANIFESTPATH, modtype .. ".list")
end

-- Return a string containing formatted modulefile text
local function moduletext(modtype, version, path)
    local header = [[
local version = "]] .. version .. [["
        
]]

    -- Template for Intel(R) oneAPI compilers
    if modtype == "oneapi" then
        return ([[
help("\nThis module makes all modules for oneAPI Toolkits\n" ..
  "available to load. It does not load any oneAPI modules\n"
)

whatis("Name: Intel(R) oneAPI Toolkit")
whatis("Description: Intel(R) oneAPI Toolkit module environment")
whatis("URL: https://www.intel.com/content/www/us/en/" .. 
    "developer/tools/oneapi/toolkits.html")

-- update module path to add oneAPI modules
prepend_path("MODULEPATH", "]] .. C.ONEAPIMODS .. [[")

]]       )
    end -- modtype=oneapi

    -- Template for Intel(R) oneAPI compilers
    if modtype == "compiler" then
        return (header .. [[
help("\nThis module loads the oneAPI compiler environment.\n" ..
  "See the man pages for icc, icpc, and ifort for detailed information\n" ..
  " on available compiler options and command-line syntax.\n" ..
  "\nVersion " .. version .. "\n"
)

whatis("Name: Intel(R) Compiler")
whatis("Version: " .. version)
whatis("Category: compiler, runtime support")
whatis("Description: Intel(R) Compiler Family (C/C++/Fortran for x86_64)")
whatis("URL: http://software.intel.com/en-us/articles/intel-compilers/")

-- update module path hierarchy
prepend_path("MODULEPATH", "]] .. C.ONEAPIMODS .. [[")
prepend_path("MODULEPATH", "]] .. C.DEPPATH .. [[/intel")

-- Assume no PAC device; allow override on each node
if not os.getenv("ACL_SKIP_BSP_CONF") then
    setenv("ACL_SKIP_BSP_CONF", "1")
end

load("compiler/" .. version)
load("mkl")

family("compiler")

]]       )
    end -- modtype=intelcompiler

    -- Template for Intel(R) oneAPI compilers
    if modtype == "gnu-mkl" then
        return (header .. [[
help("\nConfigures oneAPI MKL environment\n" ..
  "\nVersion " .. version .. "\n"
)

whatis("Name: Intel(R) Math Kernel Library")
whatis("Version: " .. version)
whatis("Category: library, runtime support")
whatis("Description: Intel(R) Math Kernel Library for C/C++ and Fortran")
whatis("URL: https://software.intel.com/en-us/en-us/intel-mkl")

-- update module path hierarchy
prepend_path("MODULEPATH", "]] .. C.ONEAPIMODS .. [[")

load("mkl/" .. version)
]]       )
    end -- modtype="gnu-mkl"

    -- Template for Intel(R) oneAPI MPI library
    if modtype == "mpi" then
        return (header .. [[
help("\nThis module loads the Intel MPI environment.\n" ..
  "   mpiifort  (Fortran source)\n" ..
  "   mpiicc    (C   source)\n" ..
  "   mpiicpc   (C++ source)\n" ..
  "\nVersion " .. version .. "\n"
)

whatis("Name: Intel MPI")
whatis("Version: " .. version)
whatis("Category: library, runtime support")
whatis("Description: Intel MPI Library (C/C++/Fortran for x86_64)")
whatis("URL: http://software.intel.com/en-us/articles/intel-mpi-library")

-- For convenience, redirect standard mpicc/mpicxx/mpifort 
-- to use oneAPI icc/icpc/ifort instead of gcc/g++/gfortran
setenv("I_MPI_CC",   "icc")
setenv("I_MPI_CXX",  "icpc")
setenv("I_MPI_FC",   "ifort")
setenv("I_MPI_F77",  "ifort")
setenv("I_MPI_F90",  "ifort")

setenv("MPI_DIR",    "]] .. PathCat(path, "mpi", version) .. [[")

prepend_path("MODULEPATH", "]] .. C.ONEAPIMODS .. [[")
prepend_path("MODULEPATH", "]] .. C.DEPPATH .. [[/intel-impi")

load("mpi/" .. version)

family("MPI")

]]       )
    end -- modtype="mpi"

    -- Template for Intel(R) oneAPI MPI library for gnu/gnuXX
    if modtype == "gnu-mpi" or modtype == "gnu" .. C.GNU .. "-mpi" then
        local gnu = modtype:gsub("-mpi","")
        return (header .. [[
    help("\nThis module loads the Intel MPI environment " ..
      "for use with ]] .. gnu .. [[\n" ..
      "   mpif90   (Fortran source)\n" ..
      "   mpicc    (C   source)\n" ..
      "   mpicxx   (C++ source)\n" ..
      "\nVersion " .. version .. "\n"
    )
    
    whatis("Name: Intel MPI")
    whatis("Version: " .. version)
    whatis("Category: library, runtime support")
    whatis("Description: Intel MPI Library (C/C++/Fortran for x86_64)")
    whatis("URL: http://software.intel.com/en-us/articles/intel-mpi-library")
    
    -- Set wrappers to GNU compilers gcc/g++/gfortran
    setenv("I_MPI_CC",   "gcc")
    setenv("I_MPI_CXX",  "gxx")
    setenv("I_MPI_FC",   "gfortran")
    setenv("I_MPI_F77",  "gfortran")
    setenv("I_MPI_F90",  "gfortran")
    
    setenv("MPI_DIR",    "]] .. PathCat(path, "mpi", version) .. [[")
    
    prepend_path("MODULEPATH", "]] .. C.ONEAPIMODS .. [[")
    prepend_path("MODULEPATH", "]] .. C.DEPPATH .. "/" .. gnu .. [[-impi")
    
    load("mpi/" .. version)
    
    family("MPI")
    
    ]]   )
    end -- modtype="gnu-mpi"

    error("Function ModuleText called with undefined filetype.", 2)
    os.exit(1)
end

-- Print command line help
local function printhelp(isfull)
    print("USAGE:\n  " .. arg[0] .. " COMMAND TYPE [arg] ")
    if isfull then
        print("\n  COMMANDs")
        for k, v in pairs(Commands) do
            if not v.ishidden then
                print(string.format("  %-10s", k) .. v.help)
            end
        end
        print("\n  TYPEs")
        for k, v in pairs(ModTypes) do
            print(string.format("  %-10s", k) .. v.help)
        end
    end
    print()
end

-- Run md5sum against file; return the (first) checksum value
local function md5sum(filename)
    local output, iserror = RunOSCommand("md5sum -z " .. filename)
    if iserror then
        printformat("Unable to create md5sum for " .. filename, "error")
    end
    return output:sub(1, 32)
end

-- Find the default version
local function find_default(path, isfullpath)
    local linkinfo = lfs.symlinkattributes(PathCat(path, "default"))
    if not linkinfo then
        return nil
    end
    if not lfs.attributes(PathCat(path, linkinfo.target), "mode") then
        printformat("Default module points to missing target " ..
            linkinfo.target, "warning")
        return nil
    end
    if linkinfo.mode ~= "link" then
        printformat(path .. " is not a link", "warning")
        return nil
    end
    if isfullpath then
        return path .. linkinfo.target
    else
        return linkinfo.target
    end
end

-- Merge results of direct file read and md5sum call
-- Return a manifest table
local function read_manifest(modtype)

    -- Convert md5sum file to array
    local function unmarshal(filename)
        local output, err = ReadFile(filename)
        if not output then
            printformat(tostring(err), "error")
        end
        local contents = {}
        for i, l in ipairs(output) do
            local m, f = l:match("^(%w*)[*%s]+(/.*)$")
            contents[i] = {}
            -- Check for saneness and avoid len(nil) error
            if (tostring(m)):len() == 32 and (tostring(f)):len() > 2 then
                contents[i].file = f
                contents[i].md5 = m
            else
                contents[i].file = nil
            end
        end
        return contents
    end

    -- Run md5sum against the manifest file; return filename/status table
    local function md5check(filename)
        local output, _ = RunOSCommand("md5sum -c --strict " ..
            filename .. " 2>/dev/null")
        if output == "" then
            printformat("md5sum error checking " .. filename, "error")
        end
        local checklist = {}
        for _, l in ipairs(StringtoArray(output)) do
            local f, s = l:match("^(/.*): +(.*)$")
            checklist[f] = "OK"
            -- Check for missing filename
            if f:len() < 3 then
                checklist[f] = "Bad filename."
            end
            -- Chck for modified file
            if s == "FAILED" then
                checklist[f] = "MOD"
            end
            --Check for missing file
            if s == "FAILED open or read" then
                checklist[f] = "Module not found."
            end
        end
        return checklist
    end

    -- Start function body
    local manifest = manifest_path(modtype)
    local modlist = setmetatable({}, Metamanifest)
    if lfs.attributes(manifest, "mode") == "file" then
        printformat("Reading manifest " .. manifest, "step")
        local contents = unmarshal(manifest)
        local checklist = md5check(manifest)
        -- Merge unmarshalled text and md5sum results into manifest table
        for i, c in ipairs(contents) do
            if not c.file then -- Bad filename
                printformat("Bad format: line " .. i, "substep")
            else
                if not checklist[c.file] then -- Filename not found
                    printformat("md5sum read error: line " .. i, "substep")
                else
                    local moddir = DirName(c.file)
                    -- Include only valid versions
                    if checklist[c.file] == "OK" or
                        checklist[c.file] == "MOD" then
                        -- Check that module type is recognized
                        if c.file == PathCat(C.MODPATH, "oneapi.lua") then
                            modlist:new("oneapi", c.file,
                                c.md5, (checklist[c.file] == "MOD"))
                        else
                            modlist:new(BaseName(c.file:gsub("%.lua$", "")),
                                c.file, c.md5, (checklist[c.file] == "MOD"))
                        end
                    else
                        printformat(c.file .. ": " .. checklist[c.file],
                            "substep")
                    end
                end
            end
        end
        return modlist
    else
        printformat("Manifest not found.", "warning")
        return nil
    end
end

-- Write a manifest
local function write_manifest(modtype, modlist)
    if dir_exists(C.MANIFESTPATH, true) then
        local contents = ""
        for _, m in pairs(modlist) do
            contents = contents .. (m.md5 .. "  " .. m.file .. "\n")
        end
        local issuccess, err = WriteFile(manifest_path(modtype), contents)
        if not issuccess then
            printformat("Cannot write manifest " .. manifest_path(modtype) ..
                "\n   " .. err, "error")
        else
            printformat("Manifest written to " ..
                manifest_path(modtype), "info")
        end
    end
    return true
end

local function find_mods(path, modtype)
    local tool = ModTypes[modtype].name
    local modlist = {}
    printformat("Searching for " .. tool .. " modules.", "info")
    local foundfiles, iserror = RunOSCommand("find " .. path ..
        " 2>/dev/null | grep -E /" .. tool .. "/[0-9]")
    if iserror then
        printformat("No modules found", "step")
        return nil
    end
    for _, l in ipairs(StringtoArray(foundfiles)) do
        printformat("Found version " .. BaseName(l), "step")
        local linkinfo = lfs.symlinkattributes(l)
        if not linkinfo then
            printformat("Not a link", "substep")
        else
            if not lfs.attributes(linkinfo.target, "mode") then
                printformat("Bad symlink", "substep")
            else
                table.insert(modlist, BaseName(l))
            end
        end
    end
    return modlist
end

local function find_tools(path, modtype)
    local mod = ModTypes[modtype]
    local toollist = {}
    for d in lfs.dir(PathCat(path, mod.name)) do
        if d:sub(1, 1) ~= "." and string.match(d:sub(1, 1), "%d") then
            local searchpath = PathCat(path, mod.name, d)
            local _, isnotfull = RunOSCommand("find " .. searchpath ..
                " 2>/dev/null | grep " .. mod.find)
            local _, isnotrt = RunOSCommand("find " .. searchpath ..
                " 2>/dev/null | grep " .. mod.rt)
            if isnotfull and not isnotrt then
                toollist[d] = "runtimes"
            end
            if not isnotfull and not isnotrt then
                toollist[d] = "full version"
            end
            if not isnotfull and isnotrt then
                toollist[d] = "incomplete install (ignored)"
            end
        end
    end
    return toollist
end

local function new_link(path, link)
    local mode = lfs.symlinkattributes(link, "mode")
    if mode and mode ~= "link" then
        printformat("Cannot create link " .. link .. "\n   " .. mode ..
            " exists", "warning")
        return false
    end
    if mode == "link" then
        local isok, err = os.remove(link)
        if not isok then
            printformat("Cannot delete link " .. link .. "\n   " ..
                tostring(err), "warning")
            return false
        end
    end
    local isok, err = lfs.link(path, link, true)
    if not isok then
        printformat("Cannot create link " .. link .. "\n   " ..
            tostring(err), "warning")
        return false
    else
        return true
    end
end

local function oneapipath(path)
    local rootpath = path
    if not path or path == "" then
        rootpath = C.ONEAPIPATH
        printformat("Using default path " .. rootpath, "info")
    else
        rootpath = path
        printformat("Using path " .. rootpath, "info")
    end
    local mode = lfs.attributes(rootpath, "mode")
    if not mode then
        printformat(rootpath .. " does not exist", "error")
    end
    if mode ~= "directory" then
        printformat(rootpath .. " is not a directory", "error")
    end
    return rootpath
end

---------------------
-- Command Methods --
---------------------

-- Output full help text
local function scripthelp(_, _)
    print(description())
    printhelp(true)
end

-- Create new modulefiles, overwriting old ones
local function update_mods(modtype, path)
    printformat("Updating oneAPI modules " .. C.ONEAPIMODS, "header")
    local rootpath = oneapipath(path)
    local oldmodlist = read_manifest(modtype) or setmetatable({}, Metamanifest)
    local modlist = setmetatable({}, Metamanifest)
    printformat("Regenerating oneAPI modulefile links", "info")
    dir_exists(C.ONEAPIMODS, true)
    local _, iserr = RunOSCommand(rootpath .. C.MODSCRIPT .. C.SCRIPTOPTS)
    if iserr then
        printformat("Cannot update oneAPI modules", "error")
    end
    -- Create base oneAPI module loader
    if modtype == "compiler" then
        dir_exists(C.MODPATH, true)
        printformat("Creating new oneAPI modulefile", "info")
        modlist:new("oneapi", PathCat(C.MODPATH, "oneapi.lua"), nil, false)
        printformat(modlist.oneapi.file, "step")
        WriteFile(modlist.oneapi.file, moduletext("oneapi", "core", rootpath))
        modlist.oneapi.md5 = md5sum(modlist.oneapi.file)
    end
    -- Create modules for all tools
    local verlist = find_mods(C.ONEAPIMODS, modtype)
    if verlist then
        printformat("Creating new " .. modtype .. " modulefiles", "info")
        dir_exists(ModTypes[modtype].path, true)
        for _, v in ipairs(verlist) do
            if oldmodlist[v] then
                printformat("Module exists, skipping: " .. v, "step")
                modlist[v] = oldmodlist[v]
            else
                modlist:new(v, PathCat(ModTypes[modtype].path, v .. ".lua"),
                    nil, false)
                printformat(modlist[v].file, "step")
                WriteFile(modlist[v].file, moduletext(modtype, v, rootpath))
                modlist[v].md5 = md5sum(modlist[v].file)
            end
        end
        local l = modlist:latest()
        if new_link(l .. ".lua",
            PathCat(ModTypes[modtype].path, "default")) then
            printformat("Default set to " .. l, "step")
        end
    end
    write_manifest(modtype, modlist)
end

-- List all modules and validate the manifest
local function list_mods(modtype, _)
    for v, m in pairs(read_manifest(modtype)) do
        local info = ""
        if m.isdiff then
            info = " (modified)"
        end
        if v == "oneapi" then
            info = " (core module)"
        end
        if v .. ".lua" == find_default(ModTypes[modtype].path, false) then
            info = info .. " (default)"
        end
        printformat(v .. " " .. info, "step")
    end
end

-- Change the default module
local function new_default(modtype, version)
    local modlist = (read_manifest(modtype))
    if modlist and modlist[version] and version ~= "oneapi" then
        if new_link(version .. ".lua", PathCat(ModTypes[modtype].path,
            "default")) then
            printformat("Default set to " .. version, "info")
        end
    else
        printformat("Version " .. version .. " doesn't exist", "error")
    end
end

-- Remove modulefiles
local function cleanup_mods(modtype, _)
    -- Delete the modules
    printformat("Deleting installed modules", "info")
    local modlist = read_manifest(modtype)
    if not modlist then
        return nil
    end
    local default = PathCat(ModTypes[modtype].path, "default")
    if lfs.symlinkattributes(default, "mode") == "link" then
        printformat("Deleting default link for " .. modtype, "step")
        if not os.remove(default) then
            printformat("Cannot delete default link for " .. default, "substep")
        end
    end
    for v, m in pairs(modlist) do
        if not m.isdiff then
            printformat("Deleting module " .. v, "step")
            if not os.remove(m.file) then
                printformat("Cannot delete module", "substep")
            end
        else
            printformat("Saving modified module " .. m.file, "info")
            BackupFIle(m.file, ".save")
        end
    end
    printformat("Deleting manifest", "step")
    if not os.remove(manifest_path(modtype)) then
        printformat("Unable to delete manifest", "substep")
    end
    if modtype == "compiler" then
        -- Recursive deletion of oneAPI module directory
        printformat("Removing oneAPI modules directory " ..
            C.ONEAPIMODS, "info")
        if C.ONEAPIMODS:len() < 15 then
            printformat("Directory target path too short, aborting for safety",
                "info")
            os.exit(1)
        else
            local _, iserror = RunOSCommand("rm -rf " .. C.ONEAPIMODS)
            if iserror then
                printformat("Cannot remove oneAPI modules directory", "warning")
            end
        end
    end
end

-- Locate and print installations under path searchpath
local function locate_all(modtype, path)
    printformat("OneAPI Installation Summary", "header")
    local rootpath = oneapipath(path)
    printformat("Searching for " .. ModTypes[modtype].name ..
        " installations.", "info")
    local toollist = find_tools(rootpath, modtype)
    if toollist == {} then
        printformat("No modules found", "step")
    else
        for k, v in pairs(toollist) do
            printformat("Found " .. v .. " " .. k, "step")
        end
    end
end

------------------
-- Main Program --
------------------

-- Define available commands and module types
Commands:new("help", "Command Help", 0, scripthelp, false)
Commands:new("update", "Create new modulefiles; optional [arg] = path",
    1, update_mods, false)
Commands:new("find", "Show installations; optional [arg] = path",
    1, locate_all, false)
Commands:new("clean", "Remove all modulefiles", 1, cleanup_mods, false)
Commands:new("list", "List current modules", 1, list_mods, false)
Commands:new("default", "Set module to default; [arg] = version",
    2, new_default, true)

-- Check for command
if not arg[1] then
    printhelp(false)
    os.exit(1)
end
-- Check for valid command
local c = arg[1]:lower()
if not Commands[c] then
    printformat("Unrecognized command: " .. arg[1], "info")
    printhelp(true)
    os.exit(1)
end
local m
if Commands[c].argc >= 1 then
    -- Check for module type
    if not arg[2] then
        printformat("Missing module type", "info")
        printhelp(true)
        os.exit(1)
    else
        -- Check for valid module type
        m = arg[2]:lower()
        if not ModTypes[m] then
            printformat("Unrecognized module type: " .. arg[2], "info")
            printhelp(true)
            os.exit(1)
        end
    end
end
-- Check for required argument
if Commands[c].argc >= 2 and not arg[3] then
    printformat("Missing argument", "info")
    printhelp(true)
    os.exit(1)
end

Commands[c].call(m, arg[3])
os.exit(0)
