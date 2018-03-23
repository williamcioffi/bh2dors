function [xxx yyy] = digi(fname)
% DIGI read in an image and digitize the data points on a graph.
%   usage: [xxx yyy] = digi(filename);
%       filename    -- full path to a supported file format.
%                      see imread for details on accepted formats.
%
%       xxx         ++ x coordinate positions in image graph scale.
%       yyy         ++ y coordinate positions in image graph scale.
%
% you will be prompted for details of in image graph axes and for the
% number of points you want to digitize in a batch.
%
% if you want to stop but you have clicked on less points than originally
% specified, press return and you will return to the number of points
% prompt.
%
%   last modified: 29Feb2016
%~wrc

% read in image
[A, MAP, ALPHA] = imread(fname);

% look at it
image(A);

%change the colorscale
colormap gray;

% digitize data using cursor input

% grab
% the origin
disp 'give me the origin';
[ox, oy] = ginput(1);

oxval = input('what is the x value at the origin ?> ');
oyval = input('what is the y value at the origin ?> ');


disp 'give me (maxx, 0)';
[maxx, oy] = ginput(1);

maxxval = input('what is the max x value ?> ');


disp 'give me (0, maxy)';
[ox, maxy] = ginput(1);

maxyval = input('what is the max y value ?> ');

x = [];
y = [];
done = 0;
while ~done
    count = input('how many points (enter 0 when done) ?> ');
    if count == 0
        done = 1;
    else
        [a, b] = ginput(count);
        x = [x a'];
        y = [y b'];
    end
end


%conversion:
yscale = (maxyval - oyval) / (oy - maxy);
xscale = (maxxval - oxval) / (maxx - ox);

yy = abs(oy - y);
xx = x - ox;

yyy = yy.*yscale + oyval;
xxx = xx.*xscale + oxval;
end