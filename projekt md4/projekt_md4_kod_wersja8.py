import struct

class MD4:

    def __init__(self, x):
        
        if len(x) > 2 ** 61:
            raise Exception("Dane wejściowe są za długie")
        if not isinstance(x, bytes) or isinstance(x, bytearray):
            raise Exception("'" + x + "'" + " nie jest instancją bytes.")

        self.x=x

        self.A= 1732584193
        self.B= 4023233417
        self.C= 2562383102 
        self.D=  271733878

        self.a= 1732584193
        self.b= 4023233417
        self.c= 2562383102 
        self.d=  271733878

        #współczynniki z tabelki
        self.y1=0
        self.z1=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
        self.w1=[3,7,11,19,3,7,11,19,3,7,11,19,3,7,11,19]

        self.y2=1518500249
        self.z2=[0,4,8,12,1,5,9,13,2,6,10,14,3,7,11,15]
        self.w2=[3,5,9,13,3,5,9,13,3,5,9,13,3,5,9,13]

        self.y3=1859775393 
        self.z3=[0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]
        self.w3=[3,9,11,15,3,9,11,15,3,9,11,15,3,9,11,15]

        self.wynik_w_bajtach=b"wartosc poczatkowa"
    
    @classmethod
    def from_string(cls, x):
    
            return cls(x.encode())

    @classmethod
    def from_file(cls, x):
         with open(x, "rb") as plik:
            
           return cls(plik.read())



    @staticmethod
    def F( x, y, z):
        return ((x&y)|((~x)&z))
    
    @staticmethod
    def G( x, y, z):
        return ((x&y)|(y&z)|(x&z))
    
    @staticmethod
    def H( x, y, z):
        return (x^y^z)
    
    @staticmethod
    def przesuniecie_bitowe_z_obrotem(a, w):
        return ((a<<w) | (a >> (32 - w)) )



    def dopelnianie_x(self):

        self.dlugosc=len(self.x)*8
        self.v= 512 - ((self.dlugosc + 65)%512)
        self.k=self.v%8
        self.f=(self.v-self.k)/8
        self.f=int(self.f)
        self.dlu_bytes=struct.pack("<Q", self.dlugosc)
        self.zapychacz=(2**self.k).to_bytes(1,'big')
        self.h= bytearray(self.f)
        self.g=(self.x + self.zapychacz + self.h + self.dlu_bytes)
        self.x=bytearray(self.g)
       


        
        
    
    def get_hash(self):

        if self.wynik_w_bajtach != b"wartosc poczatkowa":
            return ( int.from_bytes(self.wynik_w_bajtach, byteorder="big"))
        else:


            self.dopelnianie_x()
            self.licznik=len(self.x)/64
        

            for j in range(0,int(self.licznik)):
                self.a=self.A
                self.b=self.B
                self.c=self.C
                self.d=self.D


                for i in range(0,16):
                    self.a = ((self.a + self.F(self.b, self.c, self.d) + int.from_bytes(self.x[(j*64 + self.z1[i]*4):(j*64 + self.z1[i]*4 +4)], byteorder="little") + self.y1)%(2**32)) 
                    self.a= self.przesuniecie_bitowe_z_obrotem(self.a , self.w1[i])
                    self.a , self.b , self.c, self.d = self.d , self.a, self.b, self.c

                for i in range(0,16):
                    self.a = ((self.a + self.G(self.b, self.c, self.d) + int.from_bytes(self.x[(j*64 + self.z2[i]*4):(j*64 + self.z2[i]*4 +4)], byteorder="little") + self.y2) %(2**32))
                    self.a= self.przesuniecie_bitowe_z_obrotem(self.a , self.w2[i])
                    self.a , self.b , self.c, self.d = self.d , self.a, self.b, self.c

                for i in range(0,16):
                    self.a = ((self.a + self.H(self.b, self.c, self.d) + int.from_bytes(self.x[(j*64 + self.z3[i]*4):(j*64 + self.z3[i]*4 +4)], byteorder="little") + self.y3) %(2**32))
                    self.a= self.przesuniecie_bitowe_z_obrotem(self.a , self.w3[i])
                    self.a , self.b , self.c, self.d = self.d , self.a, self.b, self.c

                self.A= (self.A +self.a)%(2**32)
                self.B= (self.B +self.b)%(2**32)
                self.C= (self.C +self.c)%(2**32)
                self.D= (self.D +self.d)%(2**32)
            

            self.wynik_w_bajtach = b"".join(  [int(self.A).to_bytes(4,'little'), int(self.B).to_bytes(4,'little'), int(self.C).to_bytes(4,'little'), int(self.D).to_bytes(4,'little') ] )
    
            return( int.from_bytes(self.wynik_w_bajtach, byteorder="big"))


    #podpunkt 4
    def __str__(self):

        if self.wynik_w_bajtach != b"wartosc poczatkowa" :
            
            return (self.wynik_w_bajtach.hex())
        else:
            
            self.get_hash()
            return (self.wynik_w_bajtach.hex())

tekst=b"Ala ma kota"
ala=MD4(tekst)
print(ala.get_hash())
print(ala.get_hash())
print(ala)
print(MD4.from_string("Ala ma kota"))
