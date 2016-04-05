%normalize the numeric data.
X=X(2:162,:);

X1=X;
M=nanmean(X);
S=nanstd(X);
for i=1:size(X,1)
    for j=1:size(X,2)
        if(isnan(X1(i,j))) 
            X1(i,j)=0;
        elseif (j<8 || ( j>10 && j<181 ))
            X1(i,j)=(X1(i,j)-M(j))/S(j);
        elseif (j>180)
            X1(i,j)=X1(i,j)*.01;
        end
    end
end


for i=4:13
    text(i-3)=(txt(2,i));
end

pref_ind=[11; 45; 79; 113; 147; 181; 186; 191 ];
pref=['nCBF,'; 'nCBV,'; 'nMTT,'; 'FA,  '; 'ADC, '; 'vCBV,'; 'vADC,']; 
    
pi=1;
for i=11:190
    if (i<pref_ind(1)) pr='';
    elseif (i>=pref_ind(pi) && i<pref_ind(pi+1))
        pr=pref(pi,:);
    else
        pi=pi+1;
        pr=pref(pi,:);
    end;
    off=i-pref_ind(pi);
    if(off >=16 && off < 22)
        pr=strcat('1mm,',pr);
    elseif(off>=22 && off < 27)
        pr=strcat('2mm,',pr);
    elseif(off>28)
        pr=strcat('3mm,',pr);
    end
    arr=num2str(cell2mat(raw(2,i+3)));
    text(i)=cellstr(strcat(pr,arr));
end
OUT=cell(162,191);
OUT(1,2:191)=text;
OUT(2:162,1)=raw(3:163,3);
OUT(2:162,2:191)=num2cell(X1);



xlswrite('norm_feat.xlsx',OUT);

