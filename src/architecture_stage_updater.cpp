/* vim: ts=2:sw=2:expandtab
 *
 * VHDE - The VHDL Diagram Editor
 * Copyright (C) 2009-2017  Maurice van der Pot <griffon26@kfk4ever.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <glib.h>

#include "architecture_stage_updater.h"
#include "layout_architecture.h"

ArchitectureStageUpdater::ArchitectureStageUpdater():
  m_pArch(nullptr),
  m_pGuiInstance(nullptr),
  m_pGuiSignal(nullptr)
{
}

ArchitectureStageUpdater::~ArchitectureStageUpdater()
{
  m_pGuiSignal.reset(nullptr);
  m_pGuiInstance.reset(nullptr);
  //m_pGuiComponent.reset(nullptr);
}

void ArchitectureStageUpdater::setArchitecture(LayoutArchitecture *pArch)
{
  g_assert(m_pArch == nullptr);
  m_pArch = pArch;

}

void ArchitectureStageUpdater::setStage(Glib::RefPtr<Clutter::Stage> pStage)
{
  g_assert(!m_pStage);
  g_assert(m_pArch != nullptr);
  m_pStage = pStage;

  m_pGuiSignal = std::make_unique<GuiSignal>(m_pStage, m_pArch->getSignals()[0]);
  m_pGuiInstance = std::make_unique<GuiInstance>(m_pStage, m_pArch->getInstances()[0]);
}

