import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.SecureRandom;

public class Spritz {

  private static final int SPRITZ_N=256;

  private byte i;
  private byte j;
  private byte k;
  private byte z;
  private byte a;
  private byte w;

  private byte[] S = new byte[SPRITZ_N];

  private static enum ProgramMode {
    ENCRYPT,
    DECRYPT
  };

  private static final int IV_SIZE=10;

  public Spritz() {

    i = 0;
    j = 0;
    k = 0;
    z = 0;
    a = 0;
    w = 0;
  
    for(byte v=0; v < SPRITZ_N; v++) {
      S[v & 0xff] = v;
    }
    
  }

  private byte high(byte b) {
    return (byte) (b & 0x0F);
  }

  private byte low(byte b) {
    return (byte) ((b & 0xF0) >>> 4);
  }

  private void swap(byte a, byte b) {
    byte tmp = a;
    a = b;
    b = tmp;
  }

  private byte gcd(byte u, byte v) {
    if (u == v) {
      return u;
    }
 
    if (u == 0) {
      return v;
    }

    if (v == 0) {
        return u;
    }
 
    if ((~u & 1) == 0) { 

      if ((v & 1) == 0) {
        return gcd((byte)(u >>> 1), v);
      }
      else {
        return (byte)(gcd((byte)(u >>> 1),(byte)(v >>> 1)) << 1);
      }

    }

    if ((~v & 1) == 0) {
        return gcd(u, (byte)(v >>> 1));
    }

    if (u > v) {
        return gcd((byte)((u - v) >>> 1), v);
    }

    return gcd((byte)((v - u) >>> 1), u);
  }

  private void update() {
    i = (byte) (i + w);
    j = (byte) (k + S[(byte) (j + S[i] & 0xff)]);
    k = (byte) (i + k + S[j & 0xff]);
    swap(S[i & 0xff],S[j & 0xff]);
  }

  private void whip(int r) {
    for(int v = 0; v < r; v++) {
      update();
    }

    do {
      w = (byte) (w + 1);
    } while(gcd(w, (byte)SPRITZ_N) != (byte)1);

  }

  private void crush() {
    for(int v=0; v < (SPRITZ_N / 2); v++) {
      if(S[v & 0xff] > S[(byte)(SPRITZ_N - 1 - v) & 0xff]) {
        swap(S[v & 0xff],S[(byte)(SPRITZ_N - 1 - v) & 0xff]);
      }
    }
  }

  private void shuffle() {
    whip(SPRITZ_N * 2);
    crush();
    whip(SPRITZ_N * 2);
    crush();
    whip(SPRITZ_N * 2);
    a = 0;
  }

  private void absorb_nibble(byte x) {
    if(a == (SPRITZ_N/2)) {
      shuffle();
    }

    swap(S[a & 0xff],S[(byte)(SPRITZ_N/2 + x) & 0xff]);
    
    a = (byte) (a + 1);
  }

  private void absorb_byte(byte b) {
    absorb_nibble(low(b));
    absorb_nibble(high(b));
  }

  private void absorb(byte[] I) {
    for(byte v=0; v < I.length; v++) {
      absorb_byte(I[v]);
    }
  }
 
  private void absorb_stop() {
    if( a == SPRITZ_N/2) {
      shuffle();
    }

    a = (byte) (a + 1);
  }

  private byte drip() {
    if(a != 0) {
      shuffle();
    }

    update();

    z = S[(byte)(j + S[(byte)(i + S[(byte)(z + k) & 0xff]) & 0xff]) & 0xff];

    return z;
  }

  public static void print_usage() {
    System.out.println("Usage:\n" +
           "  spritz [mode] [key] [input file] [output file]\n" +
           "\n" + 
           "Options: \n" +
           "  mode: encrypt / decrypt\n" + 
           "  key: encryption key, may be up to 245 bytes long\n" +
           "  input file: the input to read\n" + 
           "  output file: the input to write");

  }

  public static void main(String[] args) {

    if(args.length < 3) {
      print_usage();
      System.exit(0);
    }
   
    ProgramMode program_action = null;
    byte[] key;
    byte[] iv = new byte[IV_SIZE];
    Spritz spritz;
    InputStream in = null;
    OutputStream out = null;
    int c=0;

    if(args[0].equals("encrypt")) {
      program_action = ProgramMode.ENCRYPT;
    } else if(args[0].equals("decrypt")) {
      program_action = ProgramMode.DECRYPT;
    } else {
      System.out.println("Unknown mode " + args[0] + ".");
      print_usage();
      System.exit(0);
    }

    key = args[1].getBytes();

    if(args.length >= 4) {
      try {
        in = new FileInputStream(args[2]);
      } catch(Exception ex) {
        System.out.println("Input file " + args[3] + " not found.");
        System.exit(0);
      }
    }

    if(args.length >= 5) {
      try {
        out = new FileOutputStream(args[3]);
      } catch(Exception ex) {
        System.out.println("Output file " + args[3] + " not found.");
        System.exit(0);
      }
    }

    if(program_action == ProgramMode.ENCRYPT) {
      SecureRandom random = new SecureRandom();
      random.nextBytes(iv);
      try { 
        out.write(iv); 
      } catch(Exception ex) {
        ex.printStackTrace(System.out);
        System.exit(0);
      }
    } else if (program_action == ProgramMode.DECRYPT) {
      int r = 0;
      try {
        r = in.read(iv);
      } catch(Exception ex) {
        ex.printStackTrace(System.out);
        System.exit(0);
      }
      if(r != 10) {
        System.out.println("Could not read initialisation vector.");
        System.exit(0);
      }
    }

    spritz = new Spritz();
    spritz.absorb(key);
    spritz.absorb_stop();
    spritz.absorb(iv);

    try {
      c = in.read();
    } catch(Exception ex) {
      ex.printStackTrace(System.out);
      System.exit(0);
    }
    while(c != -1) {
      byte r=0;

      if(program_action == ProgramMode.ENCRYPT) {
        r = (byte)(c + spritz.drip());
      } else if (program_action == ProgramMode.DECRYPT) {
        r = (byte)(c - spritz.drip());
      }

      try {

        out.write(r);
        c = in.read();

      } catch(Exception ex) {
        ex.printStackTrace(System.out);
      }


    }

    System.exit(0);
  }

}
