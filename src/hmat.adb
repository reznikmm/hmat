with Ada.Unchecked_Deallocation;

package body HMAT is

   --  pragma Compile_Time_Warning
   --    (Hash_Type'Modulus = 2 ** Hash_Type'Size, "Unexpected hash type");

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   function Pop_Count (Value : Unsigned_64; Bit : Bit_Index) return Bit_Count;
   --  Count 1 bits in Value (0 .. Bit) 

   Active : Change_Count := 0;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Map) is
   begin
      Active := Active + 1;
   end Adjust;

   --------------
   -- Contains --
   --------------

   function Contains (Self : Map; Key : Key_Type) return Boolean is
      Key_Hash : constant Hash_Type := Hash (Key);
      Mask     : constant Hash_Type := Hash_Type (Branches - 1);
      Next     : Node_Access := Self.Root;
      Rest     : Hash_Type := Key_Hash;
   begin
      while Next /= null loop
         if Next.Length > 0 then
            declare
               Bit : constant Bit_Index := Bit_Index (Rest and Mask);
            begin
               if (Next.Mask and 2 ** Bit) = 0 then
                  return False;
               else
                  Next := Next.Child (Pop_Count (Next.Mask, Bit));
                  Rest := Rest / Branches;
               end if;
            end;
         elsif Next.Hash = Key_Hash then
            return Equivalent_Keys (Next.Key, Key);
            -- FIXME: "Hash collision"
         else
            return False;
         end if;
      end loop;

      return False;
   end Contains;

   -------------
   -- Element --
   -------------

   function Element (Self : Map; Key : Key_Type) return Element_Type is
      Key_Hash : constant Hash_Type := Hash (Key);
      Mask     : constant Hash_Type := Hash_Type (Branches - 1);
      Next   : Node_Access := Self.Root;
      Rest     : Hash_Type := Key_Hash;
   begin
      while Next /= null loop
         if Next.Length > 0 then
            declare
               Bit : constant Bit_Index := Bit_Index (Rest and Mask);
            begin
               if (Next.Mask and 2 ** Bit) = 0 then
                  raise Constraint_Error;
               else
                  Next := Next.Child (Pop_Count (Next.Mask, Bit));
                  Rest := Rest / Branches;
               end if;
            end;
         elsif Next.Hash = Key_Hash
           and then Equivalent_Keys (Next.Key, Key)
         then
            return Next.Item;
            -- FIXME: "Hash collision"
         else
            raise Constraint_Error;
         end if;
      end loop;

      return raise Constraint_Error;
   end Element;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Map) is
   begin
      null;
   end Finalize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self : in out Map;
      Key  : Key_Type;
      Item : Element_Type)
   is
      Mask     : constant Hash_Type := Hash_Type (Branches - 1);
      Key_Hash : constant Hash_Type := Hash (Key);

      function Create_Leaf return Node_Access is
         Child : constant Node_Access := new HMAT.Node'
           (Length  => 0,
            Version => Active,
            Hash    => Key_Hash,
            Key     => Key,
            Item    => Item);
      begin
         return Child;
      end Create_Leaf;

      procedure Descent
        (Parent : in out Node_Access;
         Shift  : Bit_Count)
      is
         Suffix : constant Hash_Type := Key_Hash / 2 ** Shift;
         Slit   : constant Hash_Type := Mask * 2 ** Shift;
         Bit    : constant Bit_Index := Bit_Index (Suffix and Mask);
      begin
         if Parent = null then
            Parent := Create_Leaf;
         elsif Parent.Length > 0 then
            declare
               Index : constant Bit_Count := Pop_Count (Parent.Mask, Bit);
            begin
               if (Parent.Mask and 2 ** Bit) = 0 then
                  declare
                     Joint : constant Node_Access := new HMAT.Node
                       (Length => Parent.Length + 1);
                  begin
                     Joint.Version := Active;
                     Joint.Mask := Parent.Mask or 2 ** Bit;
                     Joint.Child (1 .. Index) := Parent.Child (1 .. Index);
                     Joint.Child (Index + 1) := Create_Leaf;
                     Joint.Child (Index + 2 .. Parent.Length + 1) :=
                       Parent.Child (Index + 1 .. Parent.Length);
                     Free (Parent);
                     Parent := Joint;
                  end;
               else
                  if Parent.Version /= Active then
                     Parent := new HMAT.Node'(Parent.all);
                     Parent.Version := Active;
                  end if;

                  Descent (Parent.Child (Index), Shift + 6);
               end if;
            end;
         elsif Parent.Hash = Key_Hash then
            if not Equivalent_Keys (Parent.Key, Key) then
               raise Program_Error with "Hash collision";
            elsif Parent.Version = Active then
               Parent.Item := Item;
            else
               Parent := Create_Leaf;
            end if;
         elsif (Parent.Hash and Slit) = (Key_Hash and Slit) then
            declare
               Joint : constant Node_Access := new HMAT.Node (Length => 1);
            begin
               Joint.Mask := 2 ** Bit;
               Joint.Child (1) := Parent;
               Parent := Joint;
               Descent (Joint.Child (1), Shift + 6);
            end;
         else
            declare
               Joint : constant Node_Access := new HMAT.Node (Length => 2);
               Bit_2 : constant Bit_Index :=
                 Bit_Index ((Parent.Hash / 2 ** Shift) and Mask);
            begin
               Joint.Mask := 2 ** Bit or 2 ** Bit_2;
               if Bit < Bit_2 then
                  Joint.Child (1) := Create_Leaf;
                  Joint.Child (2) := Parent;
               else
                  Joint.Child (1) := Parent;
                  Joint.Child (2) := Create_Leaf;
               end if;

               Parent := Joint;
            end;
         end if;
      end Descent;

   begin
      Descent (Self.Root, 0);
   end Insert;

   ---------------
   -- Pop_Count --
   ---------------

   function Pop_Count (Value : Unsigned_64; Bit : Bit_Index) return Bit_Count is
      Temp   : Unsigned_64 := Value;
      Result : Bit_Count := 0;
   begin
      for J in 0 .. Bit loop
         if (Temp and 1) /= 0 then
            Result := Result + 1;
         end if;

         Temp := Temp / 2;
      end loop;

      return Result;
   end Pop_Count;

end HMAT;
