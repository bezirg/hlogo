-- Options: max-pxcor:45, max-pycor:45, no-hwrap, no-vwrap
-- no patches, but uses links (thus also turtles)
import Language.Logo

run ["setup","go"]

setup = do
  make_node =<< nobody
  make_node =<< unsafe_turtle 0
  reset_ticks

go = forever $ do
  t <- ticks
  when (t > 1500) stop
  ask (atomic $ set_color gray) =<< unsafe_links
  make_node =<< find_partner
  tick

make_node n = do
  t <- atomic $ crt 1
  ask (do
         set_color red
         when (n /= [Nobody]) $ do
           l <- create_link_with n
           ask (atomic $ set_color green) l
           atomic $ move_to n >> fd 8) t
