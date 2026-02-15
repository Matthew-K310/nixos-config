{
  description = "NixOS configuration";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  
  outputs = { nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      username = "matthewkennedy";
    in
    {
      nixosConfigurations = {
        initium = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit username; };  # Add this
          modules = [
            ./hosts/initium/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
	      home-manager.backupFileExtension = "backup";
              home-manager.extraSpecialArgs = { inherit username; };
              home-manager.users.${username} = import ./home-manager; 
            }
          ];
        };
        
        reformata = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit username; };
          modules = [
            ./hosts/reformata/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
	      home-manager.backupFileExtension = "backup";
              home-manager.extraSpecialArgs = { inherit username; }; 
              home-manager.users.${username} = import ./home-manager; 
            }
          ];
        };
      };
    };
}
