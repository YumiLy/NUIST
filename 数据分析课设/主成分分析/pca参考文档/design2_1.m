%%%这是一个matlab help 中的主成分分析示例
% A=readtable("/Users/laiying/Downloads/data2.1.txt");
% whos
% categories
% first5 = names(1:5,:)
close all;

data21=xlsread("/Users/laiying/Downloads/data2.1.xlsx");
B=data21;
%(2:32,2:9);
figure
boxplot(B,'Labels',{'食品','衣着','居住','家庭设备及服务','交通和通讯','文教娱乐用品和服务','医疗保健','其他商品和服务'})
title('箱型图')

%算出每列的标准差进行规范化
stdr = std(B);
sr = B./repmat(stdr,31,1);%%%对数据进行规范化,其实是对相关系数矩阵进行主成分分析

[coefs,scores,variances,t2] = pca(sr);%%%对数据进行主成分分析

c3 = coefs(:,1:3) %%%主成分分析的前面三个主成分系数

% 划出前两个得分.
figure
plot(scores(:,1),scores(:,2),'+')
xlabel('1st Principal Component');
ylabel('2nd Principal Component');
gname('地区')
% gname

%%%把几个人口较多的去掉
metro = [1 9 10 11 19];
% B(metro,:)
% 
% rsubset = ratings;
% nsubset = names;
% nsubset(metro,:) = [];
% rsubset(metro,:) = [];
% size(rsubset)
% 
% %%%方差及其累积贡献
variances
percent_explained = 100*variances/sum(variances)
% 
% %%%画出pareto图
pareto(percent_explained)
xlabel('Principal Component')
ylabel('Variance Explained (%)')
% 
% 
% %%%显示结果
% biplot(coefs(:,1:2), 'scores',scores(:,1:2),'varlabels',categories);
% axis([-.26 1 -.51 .51]);



    