from slime import slime
import numpy as np
import math
import pygame
import time


class Simulation:
    def __init__(
        self,
        grid_size,
        agent_pct,
        decay=0.5,
        sensor_angle=math.radians(45),
        sensor_offset=5,
        rotation_angle=math.radians(22.5),
        step_size=1,
        deposit_amount=3,
    ):
        agent_count = math.ceil((grid_size * grid_size) * agent_pct)
        self.slime = slime()
        self.env = self.slime.init(
            decay,
            sensor_angle,
            sensor_offset,
            rotation_angle,
            step_size,
            deposit_amount,
            np.float32(np.random.random((grid_size, grid_size))),
            grid_size * np.float32(np.random.random((agent_count,))),
            grid_size * np.float32(np.random.random((agent_count,))),
            2 * math.pi * np.float32(np.random.random((agent_count,))),
        )

    def single_step(self):
        self.env = self.slime.simulation_step(self.env)

    def trail_map(self):
        return self.slime.get_trail_map(self.env).get()

    def agent_density(self):
        return self.slime.get_agent_density(self.env).get()


class SlimeQuit(Exception):
    pass


class GUI:
    def __init__(self, grid_size):
        self.grid_size = grid_size
        self.sim = Simulation(grid_size, 0.3)

    def run(self):
        pygame.init()
        pygame.display.set_caption("Slime Mold Agent Simulation")
        size = (self.grid_size, self.grid_size)
        self.screen = pygame.display.set_mode(size)
        self.surface = pygame.Surface(size)
        self.font = pygame.font.Font(None, 26)

        try:
            self.loop()
        except SlimeQuit:
            return

    def loop(self):
        while True:
            self.render()
            self.handle_input()

    def render(self):
        start = time.time()
        frame = self.new_frame()
        end = time.time()
        diff_ms = (end - start) * 1000.0

        pygame.surfarray.blit_array(self.surface, frame)
        self.screen.blit(self.surface, (0, 0))
        self.show_text("Rendered in {:.2f} ms".format(diff_ms), (5, 5))
        pygame.display.flip()

    def new_frame(self):
        self.sim.single_step()
        tm = self.sim.trail_map()
        ad = self.sim.agent_density()
        frame = np.int32(np.clip(255 * tm, 0, 255)) << 16
        frame = frame + np.int32(np.clip((25 * ad), 0, 255))
        return frame

    def show_text(self, what, where, color=(255, 0, 255), antialias=True):
        text = self.font.render(what, antialias, color)
        self.screen.blit(text, where)

    def handle_input(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise SlimeQuit()
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_q:
                    raise SlimeQuit()


if __name__ == "__main__":
    g = GUI(1000)
    g.run()
