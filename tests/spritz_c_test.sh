#!/usr/bin/env bash

rm test.*
make clean
cp main.c test.msg
make
SPRITZ_KEY=pohPa6GeiJoThe7Zaecah8rohhohheiphai8Jie7FooGiephoch3cayei5eeVo6ohka1jouH7Tahngingai3gaej7ahf3vaThaePaoCohf3suvaizopuafiash9EiquiWeir9xeich2See7cho7eer2eengahsahrae6unoozaitheiteelaiyah6teku7iebeeXae8chi0shohjai8Ahfu4owieFeeshuDiKaejex9veith
./spritz encrypt $SPRITZ_KEY test.msg test.encrypted
./spritz decrypt $SPRITZ_KEY test.encrypted test.decrypted
PLAINTEXT_SUM=`sha1sum test.msg | awk '{print $1}'`
DECRYPTED_SUM=`sha1sum test.decrypted | awk '{print $1}'`

if [ $PLAINTEXT_SUM == $DECRYPTED_SUM ]; then
  echo "self test passed."
else
  echo "self test failed, exiting with test files intact."
  exit -1
fi

rm test.encrypted
rm test.decrypted

./tests/spritzsaber encrypt $SPRITZ_KEY test.msg test.encrypted
./spritz decrypt $SPRITZ_KEY test.encrypted test.decrypted

DECRYPTED_SUM=`sha1sum test.decrypted | awk '{print $1}'`

if [ $PLAINTEXT_SUM == $DECRYPTED_SUM ]; then
  echo "reference test passed."
else
  echo "reference test failed, exiting with test files intact."
  exit -1
fi

rm test.encrypted
rm test.decrypted

rm test.msg
