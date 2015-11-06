-- Options: max-pxcor:45, max-pycor:45, no-hwrap, no-vwrap
-- no patches, but uses links (thus also turtles)
import Language.Logo

patches_own []
turtles_own []
links_own []

setup = do
  make_node =<< nobody
  make_node =<< unsafe_turtle 0

go = forever $ do
  t <- unsafe_ticks
  when (t > 1500) stop
  ask (atomic $ set_color gray) =<< unsafe_links
  make_node =<< find_partner
  atomic $ tick

make_node n = do
  t <- atomic $ crt 1
  ask (do
         set_color red
         when (n /= [Nobody]) $ do
           l <- create_link_with n
           ask (atomic $ set_color green) l
           atomic $ move_to n >> fd 8) t
