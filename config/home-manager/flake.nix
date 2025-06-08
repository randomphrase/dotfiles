{
  description = "Home Manager configuration of alastair";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, home-manager, ... }: {
      homeConfigurations = {
	"alastair@berry" = home-manager.lib.homeManagerConfiguration ({
	  modules = [ (import ./home.nix) ];
	  pkgs = import nixpkgs {
	    system = "x86_64-linux";
	  };
	});

	"alastair@Alastairs-MBP" = home-manager.lib.homeManagerConfiguration ({
	  modules = [ (import ./home.nix) ];
	  pkgs = import nixpkgs {
	    system = "aarch64-darwin";
	  };
	});
      };
    };
}
