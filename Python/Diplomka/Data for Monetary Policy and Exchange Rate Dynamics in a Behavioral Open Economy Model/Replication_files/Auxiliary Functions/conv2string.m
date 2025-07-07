function vars_string = conv2string(vars_cell_array)

%Converts cell array list of variables in vars_cell_array into string with spaces

vars_string = [];

%Hard to preallocate as length unclear
for i = 1:numel(vars_cell_array)
    vars_string = [vars_string,' ',vars_cell_array{i}];
end
end