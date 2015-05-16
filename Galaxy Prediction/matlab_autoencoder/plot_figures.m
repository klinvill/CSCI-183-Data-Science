function plot_figures(W,X,b,bT,count,cost)
     figure(1);
     x1=(W'*max(W*X(:,8)+b,0)+bT);
     subplot(2,3,1);
     pcolor(reshape(double(x1),30,30));
     shading flat;
     subplot(2,3,2);
     pcolor(reshape(double(X(:,8)),30,30));
     shading flat;
     subplot(2,3,3);
     plot(cost(1:count));
     x1=(W'*max(W*X(:,23)+b,0)+bT);
     subplot(2,3,4);
     pcolor(reshape(double(x1),30,30));
     shading flat;
     subplot(2,3,5);
     pcolor(reshape(double(X(:,23)),30,30));
     shading flat;
     
     figure(2);
     for q=1:100
         subplot(10,10,q);
         pcolor(reshape(double(W(q,:)),30,30));
         shading flat;
     end
end
     