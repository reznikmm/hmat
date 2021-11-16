with HMAT;
with Ada.Containers;
--  with Ada.Text_IO;


procedure Main is
--   package Hash_IO is new Ada.Text_IO.Modular_IO (Ada.Containers.Hash_Type);

   function Hash (V : Ada.Containers.Hash_Type) return Ada.Containers.Hash_Type is (V);

   package Maps is new HMAT
     (Ada.Containers.Hash_Type,
      Positive,
      Ada.Containers.Hash_Type,
      Hash,
      Ada.Containers."=");

begin
   declare
      Map : Maps.Map;
   begin
      Map.Insert (8#01_01_01_01_01#, 1);  --  (1)
      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      Map.Insert (8#02_02_02_02_02#, 2);  --  [1: 1, 2: 2]
      pragma Assert (Map.Contains (8#02_02_02_02_02#));
      Map.Insert (8#03_03_03_03_03#, 3);  --  [1: 1, 2: 2, 3: 3]
      Map.Insert (8#04_04_04_04_01#, 4);  --  [1: [1,4] 2: 2, 3: 3]
      Map.Insert (8#05_02_02_02_02#, 5);  --  [.. 2: [2:[2:[2:5]]] ...]
      Map.Insert (8#01_01_01_01_03#, 6);  --  [1: 1, 2: 2, 3: 3]
      Map.Insert (8#01_01_01_01_01#, 7);

      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      pragma Assert (Map.Contains (8#02_02_02_02_02#));
      pragma Assert (Map.Contains (8#03_03_03_03_03#));
      pragma Assert (Map.Contains (8#04_04_04_04_01#));
      pragma Assert (Map.Contains (8#05_02_02_02_02#));
      pragma Assert (Map.Contains (8#01_01_01_01_03#));

      pragma Assert (Map.Element (8#01_01_01_01_01#) = 7);
      pragma Assert (Map.Element (8#02_02_02_02_02#) = 2);
      pragma Assert (Map.Element (8#03_03_03_03_03#) = 3);
      pragma Assert (Map.Element (8#04_04_04_04_01#) = 4);
      pragma Assert (Map.Element (8#05_02_02_02_02#) = 5);
      pragma Assert (Map.Element (8#01_01_01_01_03#) = 6);
   end;

   declare
      Map, Map_2 : Maps.Map;
   begin
      Map_2.Insert (8#01_01_01_01_01#, 1);  --  (1)
      Map := Map_2;
      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      Map.Insert (8#02_02_02_02_02#, 2);  --  [1: 1, 2: 2]
      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      pragma Assert (Map.Contains (8#02_02_02_02_02#));
      pragma Assert (not Map_2.Contains (8#02_02_02_02_02#));
   end;

   declare
      Map, Map_2 : Maps.Map;
   begin
      Map_2.Insert (8#01_01_01_01_01#, 1);  --  (1)
      Map_2.Insert (8#02_02_02_02_01#, 2);  --  [1: [1:1, 2: 2]]
      Map := Map_2;
      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      pragma Assert (Map.Contains (8#02_02_02_02_01#));
      Map.Insert (8#03_03_03_03_01#, 3);  --  [1: [1:1, 2: 2, 3: 3]]
      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      pragma Assert (Map.Contains (8#02_02_02_02_01#));
      pragma Assert (Map.Contains (8#03_03_03_03_01#));
      pragma Assert (not Map_2.Contains (8#03_03_03_03_01#));
   end;

   declare
      Map, Map_2 : Maps.Map;
   begin
      Map_2.Insert (8#01_01_01_01_01#, 1);  --  (1)
      Map_2.Insert (8#02_02_02_02_01#, 2);  --  [1: [1:1, 2: 2]]
      Map := Map_2;
      pragma Assert (Map.Contains (8#01_01_01_01_01#));
      pragma Assert (Map.Contains (8#02_02_02_02_01#));
      Map.Insert (8#01_01_01_01_01#, 3);  --  [1: [1:3, 2: 2]]
      pragma Assert (Map.Element (8#01_01_01_01_01#) = 3);
      pragma Assert (Map.Element (8#02_02_02_02_01#) = 2);
      pragma Assert (Map_2.Element (8#01_01_01_01_01#) = 1);
      pragma Assert (Map_2.Element (8#02_02_02_02_01#) = 2);
   end;
end Main;
