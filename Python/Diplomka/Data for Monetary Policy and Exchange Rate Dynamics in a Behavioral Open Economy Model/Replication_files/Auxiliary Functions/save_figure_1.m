%Trim and save a figure in requested format

%Resize so that pdf of plots looks ok
set(gcf,'Position',[208 0 500 1080]);

%Export figure to file if requested
set(f,'Units','Inches');
pos = get(f,'Position');

%Disable Matlab size warning
warning('off','MATLAB:print:FigureTooLargeForPage');
        
%Trim and print to pdf
set(f,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3),.75*pos(4)]);
print(f,[export_name,'.',export_fmt],['-d',export_fmt],'-r0');