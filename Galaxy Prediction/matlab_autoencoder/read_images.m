imin=112;
imax=424-imin-1;
sdirectory = 'images';
jpgfiles = dir([sdirectory '/*.jpg']);

X=zeros(length(jpgfiles),900);

for k = 1:length(jpgfiles)
filename = [sdirectory '/' jpgfiles(k).name];
filename

I = imread(filename);
I=rgb2gray(I);
J=I(imin:imax,imin:imax);
K=imresize(J,[30 30]);
X(k,:)=reshape(K,1,900);
imshow(K);
end

X=single(X);

save 'X.mat' X;
