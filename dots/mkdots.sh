for file in *.dot
do
  dot -Tpng ${file} > ${file}.png
done

convert -delay 100 -loop 0 *.png movie.gif