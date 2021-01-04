{ nginxWebRoot }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  services = {
    openssh.enable = true;
    nginx = {
      enable = true;
      virtualHosts."ftzm.org" = {
        enableACME = true;
        forceSSL = true;
        root = "${nginxWebRoot}";
        locations = {
          "/" = {
            extraConfig = ''
               # hide .html ending
               if ($request_uri ~ ^/(.*)\.html$) {
		               return 302 $scheme://$http_host/$1;
               }
               try_files $uri $uri.html $uri/ =404;

               # redirect to articles from index
               # location = /index.html {return 302 $scheme://$http_host/articles;}
            '';
          };
        };
        extraConfig = ''
	        error_page 404 /404.html;
        '';
      };
    };
  };

  security.acme = {
    acceptTerms = true;
    certs = {
      "ftzm.org" = {
        email = "fitz.matt.d@gmail.com";
        webroot = "/var/lib/acme/acme-challenge";
        group = "nginx";
      };
    };
  };

  boot.cleanTmpDir = true;
  networking = {
    hostName = "einmana";
    firewall = {
      allowPing = true;
      allowedTCPPorts = [ 80 443 ];
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ matt@utlendingur"
  ];
}
