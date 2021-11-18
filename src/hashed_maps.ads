with Ada.Finalization;

generic
   type Key_Type is private;
   type Element_Type is private;
   type Hash_Type is mod <>;

   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

package Hashed_Maps is
   type Map is tagged private;

   procedure Insert
     (Self : in out Map;
      Key  : Key_Type;
      Item : Element_Type);

   function Contains (Self : Map; Key : Key_Type) return Boolean;
   function Element (Self : Map; Key : Key_Type) return Element_Type;

private

   Branches : constant := 64;

   type Unsigned_64 is mod 2 ** Branches;
   subtype Bit_Count is Natural range 0 .. Branches;
   subtype Bit_Index is Natural range 0 .. Branches - 1;
   type Node;

   type Node_Access is access all Node;
   type Node_Access_Array is array (Bit_Count range <>) of Node_Access;

   type Change_Count is mod 2 ** 32;

   type Node (Length : Bit_Count) is record
      Version : Change_Count;
      Counter : Natural;

      case Length is
         when 0 =>
            Hash : Hash_Type;  --  Hash (Key)
            Key  : Key_Type;
            Item : Element_Type;
         when 1 .. Branches =>
            Mask  : Unsigned_64;
            Child : Node_Access_Array (1 .. Length);
      end case;
   end record;

   type Map is new Ada.Finalization.Controlled with record
      Root : Node_Access;
   end record;

   overriding procedure Adjust (Self : in out Map);

   overriding procedure Finalize (Self : in out Map);

end Hashed_Maps;