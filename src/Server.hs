import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

import Board
import Draw
import Input
import AI

main = withSocketsDo $
	do 	addrInfos <- getAddrInfo
					(Just (defaultHints {addrFlags = [AI_PASSIVE]}))
					Nothing (Just "48631")
		let serverAddr = head addrInfos

		--create socket
		s <- socket (addrFamily serverAddr) Stream defaultProtocol

		--bind to an address/port to listen to
		bind s (addrAddress serverAddr)

		--only allow 1 active connection
		listen s 1



		--loop forever
		processRequest s

	where 
		processRequest :: Socket -> IO ()
		processRequest s = 
			do 	(conn, address) <- accept s
				inputByteString <- recv conn 65536
				print "Got message"
				let outputByteString = encode $ updateWorld 1 $ decode inputByteString
					in sendAll conn outputByteString
				processRequest s
	



--main = do
--	s <- listenOn (PortNumber $ fromIntegral 12345)
--	loop s

--loop s = do
--	(input, host, port) <- accept s
--	str <- recvFrom host port
--	print str
--	loop s