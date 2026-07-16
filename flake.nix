{
  description = "Home Manager configuration of alastair";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util.url = "github:hraban/mac-app-util";
  };

  outputs =
    { self, nixpkgs, home-manager, mac-app-util, ... }: {
      homeConfigurations = {
	"alastair@berry" = home-manager.lib.homeManagerConfiguration ({
	  modules = [ (import ./home.nix) ];
	  pkgs = import nixpkgs {
	    system = "aarch64-linux";
	  };
	});

	"alastair@thinky" = home-manager.lib.homeManagerConfiguration ({
	  modules = [ (import ./home.nix) ];
	  pkgs = import nixpkgs {
	    system = "x86_64-linux";
	  };
	  extraSpecialArgs = { withGui = true; };
	});

	"alastair@Alastairs-MBP" = home-manager.lib.homeManagerConfiguration ({
	  modules = [
	    (import ./home.nix)
	    mac-app-util.homeManagerModules.default
	  ];
	  pkgs = import nixpkgs {
	    system = "aarch64-darwin";
	  };
	});

	"alastair@us01" = home-manager.lib.homeManagerConfiguration ({
	  modules = [ (import ./home.nix) ];
	  pkgs = import nixpkgs {
	    system = "x86_64-linux";
	  };
	});

        "SenorAl@tt-quietbox" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
          };
          modules = [
            ./home.nix
            {
              home.username = nixpkgs.lib.mkForce "SenorAl";
            }
          ];
        };
      };
    };
}
