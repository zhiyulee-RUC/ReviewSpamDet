ReviewSpamDet
=========================
本代码库是《基于评论产品属性情感倾向评估的虚假评论识别研究》论文中涉及的相关代码、参考资源以及词库的Git仓库，您可以根据您的需要进行相应的下载操作，也可以将您的意见和建议反馈给我们，谢谢！

> 论文作者联系方式：zhiyulee@icloud.com
  
该Git仓库主要包含以下几个部分：

- Coffee_Reviews.xlsx
- Emo.dic.xlsx
- Other_dic
- RCode_for_RAPBEE_model.R
- Rwordseg

Coffee_Reviews.xlsx
=========

Coffee_Reviews.xlsx主要包含了论文实验中所用到的评论库，该库主要由taobao.rank、time.rank、Review.ID、Review.Credit、Review.content、Review.time、Review.time.min、Review.eva、Review.vote、Review.wordscount字段组成；分别为如下含义：	
 - taobao.rank: 淘宝中的推荐排序
 - time.rank: 相对时间顺序
 - Review.ID: 用户昵称
 - Review.Credit: 用户淘宝信誉
 - Review.content: 用户评论内容
 - Review.time: 用户评论时间
 - Review.time.min: 用户评论时间精确到分钟
 - Review.eva: 用户评论类型（淘宝给定：好评、中评、差评）
 - Review.vote: 用户投票
 - Review.wordscount: 用户评论字数


Emo.dic.xlsx
================

Emo.dic.xlsx为词语情感词典；该情感词典为专门针对在线商品评论进行优化后的情感倾向词典，去除了原有一般情感词典中不常用的词语，并且添加了大量的和在线购物相关的用户用以表达情感倾向的词汇，是目前用于用户在线商品评论情感分析的较好情感词库；  

该词库主要包含三个字段：
 - Words: 词语本身
 - Polaeity: 基本极性
 - Strength: 词语极性强度



Gen.Att.dic
================
评论相关基本属性词典，该属性词典是对评论中涉及到的评论属性特征词做出的基本分类；主要包含4个大类及其13个小类；

 - 服务评价
- 客户服务
- 发货物流服务
- 包装服务
 - 产品评价
- 使用体验
- 尺寸规格
- 产品质量
- 产品价格
- 产品描述
 - 指导性评价
- 对比性评价
- ”评论“评价
- 推荐与告诫
 - 整体性评价
- 代买赠送
- 泛指评价

Other_dic
================
Other_dic主要包含killwords.txt和stopwords.txt两个部分，主要初步预处理评论内容增加算法的稳定性；

- killwords.txt: 无关剔除
- stopwords.txt: 常见停用词去除


RCode\_for\_RAPBEE_model.R
===============
RCode\_for\_RAPBEE_model.R为程序的核心代码，采用R语言完成；下载修改成相应的系统格式即可运行，代码采用了良好的缩进以及备注，易于读者理解；

Rwordseg
========
Rwordseg 是一个R环境下的中文分词工具，使用rJava调用Java分词工具Ansj。

Ansj 也是一个开源的 Java 中文分词工具，基于中科院的 ictclas 中文分词算法，采用隐马尔科夫模型（Hidden Markov Model, HMM）。作者孙健重写了一个Java版本，并且全部开源，使得 Ansi 可用于人名识别、地名识别、组织机构名识别、多级词性标注、关键词提取、指纹提取等领域，支持行业词典、 用户自定义词典。详细信息可以参考作者孙健的专访以及项目的Github地址：https://github.com/ansjsun/ansj_seg


说明
==========

参考使用本文代码以及词典请于论文中标注说明或引用，谢谢！作者保持对本文的最终解释权！

&copy; 2014 RUC.Info and CCNU.InfoM. All rights Reserved. 