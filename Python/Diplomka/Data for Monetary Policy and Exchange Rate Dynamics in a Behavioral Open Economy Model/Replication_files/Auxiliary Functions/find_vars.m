%This code allocates numbers to variables (these should match dynare internal ordering)

where_de    = strmatch('de',var_list_,'exact');
where_rho   = strmatch('rho',var_list_,'exact');
where_id1   = strmatch('id1',var_list_,'exact');
where_q     = strmatch('q',var_list_,'exact');
where_id0   = strmatch('id0',var_list_,'exact');
 
where_id = NaN(1,j+1);

for n = 0:j
    where_id(n+1) = strmatch(['id',num2str(n)],var_list_,'exact');
end