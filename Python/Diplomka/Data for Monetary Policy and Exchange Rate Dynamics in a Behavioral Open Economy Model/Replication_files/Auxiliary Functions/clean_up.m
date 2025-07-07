%----------------------------------------------------------------------%
%                                                                      %
%                               CLEAN UP                               %
%                                                                      %
%----------------------------------------------------------------------%

%Get list of all files starting with the string OE (except for mod files)
all_files=dir([pwd,'\OE*']); all_names = {all_files.name};
     
%Get list of all mod files
mod_files = dir('*.mod');    mod_file_names = {mod_files.name};
        
%Names of files to be deleted (exclude .mod files themselves)
tbd = setdiff(all_names,mod_file_names);
        
%Delete post-compilation files and directories
for ii = 1:numel(tbd)
    if isfolder(tbd{ii})
        rmdir(tbd{ii},'s');
    elseif isfile(tbd{ii})
        delete(tbd{ii});
    end
end