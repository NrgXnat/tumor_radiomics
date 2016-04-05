function[]=radiom()

[X txt raw]=xlsread('D:\CONDR_METS\radiomic_features_2_no_exclusions.xlsx','Sheet1');
%[X txt raw]=xlsread('C:\Users\mmilch01\Desktop\CONDR_METS\radiomic_features.xlsx','Sheet1_upd');

%delete 1st row from X.
%X(1,:)=[];

%normalize the numeric data.
rows=size(raw,1)-1;
cols=190;
ns=5; ne=194;
%X=cell2mat(raw(3:rows,nums:nume));

sgeom=[1];
egeom=[7];
satl=[8];
eatl=[10];

%histogram measures
shist=[11 45 79 113 147];
ehist=shist+15;
%texture measures
st1=[27 61 95 129 163];
se1=st1+5;
st2=se1+1;
se2=st2+5;
st3=se2+1;
se3=st3+5;
st=[st1 st2 st3];
se=[se1 se2 se3];
%volume fraction.
svf=[181 186];
evf=svf+4;

%percentiles
sperc=shist+4;
eperc=sperc+8;

X1=X;
M=nanmean_(X);
S=nanstd_(X);
no_norm=erange(X,satl,eatl);

for i=1:size(X,1)
    for j=1:size(X,2)        
%        if(isnan(X1(i,j))) 
%            X1(i,j)=0;
%        end;
        if (~no_norm(j) || isnan(X1(i,j))) continue; end;
        %normalise
        X1(i,j)=(X1(i,j)-M(j))/S(j);
    end
end

for i=ns:eatl(1)+ns-1
    text(i-ns+2)=(txt(1,i));
end

pref_ind=[11; 45; 79; 113; 147; 181; 186; 191 ];
pref=['nCBF_'; 'nCBV_'; 'nMTT_'; '  FA_'; ' ADC_'; 'vCBV_'; 'vADC_'];
    
pi=1;
text(1)=cellstr('lesion');
for i=shist:cols
    pr='';
    arr=num2str(cell2mat(raw(1,i+ns-1)));
    text(i+1)=cellstr(strcat(pr,arr));
end
text(cols+2)=cellstr('origin');
OUT=cell(rows+1,cols+2);
%Variable headers
OUT(1,1:cols+2)=text;
%First column -labels
OUT(2:rows+1,1)=raw(2:rows+1,ns-1);
%Last column - hist. origin
OUT(2:rows+1,cols+2)=raw(2:rows+1,cols+ns);
%Numerical values
OUT(2:rows+1,2:cols+1)=num2cell(X1(:,1:cols));

%all
xlswrite('norm_feat.xls',OUT,'all');
%all, no perc.
xlswrite('norm_feat.xls',cutout(OUT,sperc,eperc),'all_noperc');
%geom
xlswrite('norm_feat.xls',cutin(OUT,sgeom,egeom),'geom');
%atl
xlswrite('norm_feat.xls',cutin(OUT,satl,eatl),'atl');

%hist
xlswrite('norm_feat.xls',cutin(OUT,shist,ehist),'hist');
xlswrite('norm_feat.xls',cut(OUT,shist,ehist,sperc,eperc),'hist_noperc');

%texture
xlswrite('norm_feat.xls',cutin(OUT,st,se),'texture');
xlswrite('norm_feat.xls',cutin(OUT,st1,se1),'texture1');
xlswrite('norm_feat.xls',cutin(OUT,st2,se2),'texture2');
xlswrite('norm_feat.xls',cutin(OUT,st3,se3),'texture3');
%atl+geom
xlswrite('norm_feat.xls',cutin(OUT,sgeom,eatl),'geom_atl');


% # Columns 1-10: geometry measures.
% # 1-vol(mm3), 2-nvox, 3-longest diam(mm), 4-shortest diam(mm), 5-eccentricity, 6-sphericity, 7-compactness,8-atl_x, 9-atl_y, 10-atl_z
% 
% # Columns 11-26: nCBF histogram statistics.
% # 11: nCBF mean, 12-nCBF min 13-nCBF max 14-nCBF stddev, 15-pcl 1, 16-pcl 5, 17-pcl 10, 18-pcl 25, 19-pcl 50, 20-pcl 75, 
% # 21-pcl 90, 22-pcl 95, 23-pcl 99, 24-entropy, 25-skewness, 26-curtosis
% # Columns 27-32: nCBF texture statistics (rotation invariant 1mm scale).
% # 27-contrast, 28-dissimilarity, 29-correlation, 30-energy, 31-homogeneity, 32-entropy
% # Columns 33-38: nCBF texture statistics (rotation invariant 2mm scale): same as 1mm
% # Columns 39-44: nCBF texture statistics (rotation invariant 3mm scale): same as 1mm
% # Columns 45-78: nCBV histogram & texture statistics (same).
% # Columns 79-112: nMTT histogram & texture statistics (same).
% # Columns 113-146: FA histogram & texture statistics (same).
% # Columns 147-180: MD/ADC histogram & texture statistics (same).
% 
% # Columns 181-85: nCBV thresholded volume fraction. 181-1, 182-1.5, 183-2, 184-3, 185-5
% # Columns 186-190: MD/ADC thresholded volume fraction. 186-.5, 197-.75, 198-1, 199-1.25, 190-1.5

%texture CBF 2mm
%X2=X1(:,33:38);

%texture CBF 3mm
%X2=X1(:,39:44);

%texture CBF 1mm
%X2=X1(:,27:32);

%d='euclidean';
%Z=linkage(X2,'ward',d);
%T=cluster(Z,'cutoff',1.15);
%subplot(1,2,1);
%dendrogram(Z,'colorthreshold','default');
%subplot(1,2,2);
%silhouette(X2,T);

function m = nanmean_(x,dim)
nans = isnan(x);
x(nans) = 0;

if nargin == 1 % let sum deal with figuring out which dimension to use
    % Count up non-NaNs.
    n = sum(~nans);
    n(n==0) = NaN; % prevent divideByZero warnings
    % Sum up non-NaNs, and divide by the number of non-NaNs.
    m = sum(x) ./ n;
else
    % Count up non-NaNs.
    n = sum(~nans,dim);
    n(n==0) = NaN; % prevent divideByZero warnings
    % Sum up non-NaNs, and divide by the number of non-NaNs.
    m = sum(x,dim) ./ n;
end

function y = nanstd_(varargin)
y = sqrt(nanvar_(varargin{:}));

function y = nanvar_(x,w,dim)

if nargin < 2 || isempty(w), w = 0; end

sz = size(x);
if nargin < 3 || isempty(dim)
    % The output size for [] is a special case when DIM is not given.
    if isequal(x,[]), y = NaN(class(x)); return; end

    % Figure out which dimension sum will work along.
    dim = find(sz ~= 1, 1);
    if isempty(dim), dim = 1; end
elseif dim > length(sz)
    sz(end+1:dim) = 1;
end

% Need to tile the mean of X to center it.
tile = ones(size(sz));
tile(dim) = sz(dim);

if isequal(w,0) || isequal(w,1)
    % Count up non-NaNs.
    n = sum(~isnan(x),dim);

    if w == 0
        % The unbiased estimator: divide by (n-1).  Can't do this when
        % n == 0 or 1, so n==1 => we'll return zeros
        denom = max(n-1, 1);
    else
        % The biased estimator: divide by n.
        denom = n; % n==1 => we'll return zeros
    end
    denom(n==0) = NaN; % Make all NaNs return NaN, without a divideByZero warning

    x0 = x - repmat(nanmean_(x, dim), tile);
    y = nansum_(abs(x0).^2, dim) ./ denom; % abs guarantees a real result

% Weighted variance
elseif numel(w) ~= sz(dim)
    error(message('stats:nanvar:InvalidSizeWgts'));
elseif ~(isvector(w) && all(w(~isnan(w)) >= 0))
    error(message('stats:nanvar:InvalidWgts'));
else
    % Embed W in the right number of dims.  Then replicate it out along the
    % non-working dims to match X's size.
    wresize = ones(size(sz)); wresize(dim) = sz(dim);
    wtile = sz; wtile(dim) = 1;
    w = repmat(reshape(w, wresize), wtile);

    % Count up non-NaNs.
    n = nansum(~isnan(x).*w,dim);

    x0 = x - repmat(nansum_(w.*x, dim) ./ n, tile);
    y = nansum_(w .* abs(x0).^2, dim) ./ n; % abs guarantees a real result
end

function y = nansum_(x,dim)
x(isnan(x)) = 0;
if nargin == 1 % let sum figure out which dimension to work along
    y = sum(x);
else           % work along the explicitly given dimension
    y = sum(x,dim);
end

%inclusive range
function [r]=irange(x,st,en)
l=size(x,2);
r=zeros(l,1);
for i=1:size(st(:))
    r(st(i):en(i))=1;
end

%exclusive range
function [r]=erange(x,st,en)
l=size(x,2);
r=ones(l,1);
for i=1:size(st(:))
    r(st(i):en(i))=0;
end

function[R] = cut(IN,stin,enin,stout,enout)
t=erange(IN(:,2:size(IN,2)),stin,enin);
t1=irange(IN(:,2:size(IN,2)),stout,enout);
rng=find(bitor(t,t1))+1;
R=IN;
R(:,rng)=[];
R(:,size(R,2)+1)=IN(:,size(IN,2));

%keep specified columns only.
function[R] = cutin(OUT,st,en)
t=erange(OUT(:,2:size(OUT,2)),st,en);
rng=find(t)+1;
R=OUT;
R(:,rng)=[];
%keep the last col.
R(:,size(R,2)+1)=OUT(:,size(OUT,2));

%delete unnecessary columns.
function[R] = cutout(OUT,st,en)
rng=find(irange(OUT,st,en))+1;
rng=[ rng(:)' size(OUT,2) ];
R=OUT;
R(:,rng)=[];
%keep the last col.
R(:,size(R,2)+1)=OUT(:,size(OUT,2));

