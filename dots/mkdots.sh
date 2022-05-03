mkdir -p png
for file in *.dot
do
  dot -Tpng ${file} > "png/${file}.png"
done

convert -delay 30 -loop 0 png/*.png movie.gif