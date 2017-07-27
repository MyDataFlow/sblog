简单博客
===
 [x] 文章CURD   
 [x] 书签CURD   
 [x] OpenGraph    
 [x] Sitemaps   
 [x] RSS  
 [ ] CSRF

使用环境
---
 1.Ubuntu 16.04.2 LTS   
 2.GHC version 7.10.3   
 3.cabal-install version 1.22.6.0   
 4.PostgreSQL 9.5

如何编译
---
 1.git clone https://github.com/DavidAlphaFox/sblog.git   
 2.cabal sandbox init   
 3.cabal install    
 4.cabal build

如何使用
---
 1.更新ga.js文件中的关于Google Analytics的代码   
 2.重新配置application.conf，设置管理员密码，数据库配置   
 3.导入dump.sql   
 4.配置nginx作为sblog的代理和静态文件服务器    
 5.配置systemd并添加自启动
