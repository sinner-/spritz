#!/usr/bin/env bash

rm test.*
rm Spritz.class
cp main.c test.msg
javac Spritz.java
SPRITZ_KEY=pohPa6GeiJoThe7Zaecah8rohhohheiphai8Jie7FooGiephoch3cayei5eeVo6ohka1jouH7Tahngingai3gaej7ahf3vaThaePaoCohf3suvaizopuafiash9EiquiWeir9xeich2See7cho7eer2eengahsahrae6unoozaitheiteelaiyah6teku7iebeeXae8chi0shohjai8Ahfu4owieFeeshuDiKaejex9veith
PLAINTEXT_SUM=`sha1sum test.msg | awk '{print $1}'`

./tests/spritzsaber encrypt $SPRITZ_KEY test.msg test.encrypted
java Spritz decrypt $SPRITZ_KEY test.encrypted test.decrypted

DECRYPTED_SUM=`sha1sum test.decrypted | awk '{print $1}'`

if [ -z $DECRYPTED_SUM ]; then
  echo "decryption failed. exiting."
  exit -1
fi

if [ $PLAINTEXT_SUM == $DECRYPTED_SUM ]; then
  echo "reference test passed."
else
  echo "reference test failed, exiting with test files intact."
  exit -1
fi

rm test.encrypted
rm test.decrypted

rm test.msg
