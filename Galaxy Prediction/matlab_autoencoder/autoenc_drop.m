function [xp dW db dbT]=autoenc_drop(x,W,b,bT)
batch_size=size(x,2);
xe=x;


% add noise
x=x+randn(size(x))*.01;


hidden_size=size(W,1);

% feed forward
h=W*x;
h=h+repmat(b,1,batch_size);
h=max(h,0);

xp=W'*h;
xp=xp+repmat(bT,1,batch_size);
  

% compute error
d=xp-xe;
dW=h*d'/batch_size;
db=mean(xp-x,2);

% compute derivative of error using chain rule
df=drectlin(h);
dT=zeros(size(h));
for i=1:hidden_size
    dT(i,:)=(W(i,:)*d).*df(i,:);
end

dWT=x*dT'/batch_size;
dbT=mean(dT,2);

dW=(dWT'+dW);



end

function X = drectlin(P)
    X=zeros(size(P));
    X(P>0)=1;
end