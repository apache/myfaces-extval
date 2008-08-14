/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 */
public abstract class AbstractStartupConfigListener implements PhaseListener
{
    protected final Log logger = LogFactory.getLog(getClass());
    //don't remove - it's a fallback if there is a problem with deregistration
    //target: don't process init logic more than once
    private static List<Class> initializedListeners = new ArrayList<Class>();

    public void afterPhase(PhaseEvent event)
    {
    }

    public void beforePhase(PhaseEvent event)
    {
        synchronized (AbstractStartupConfigListener.class)
        {
            if (!initializedListeners.contains(getClass()))
            {
                try
                {
                    init();

                    ExtValUtils.deregisterPhaseListener(this);
                }
                catch (Throwable t)
                {
                    this.logger
                            .warn(
                                    "an exception occurred while deregistering the phase-listener"
                                            + getClass().getName()
                                            + " -> there is just a little overhead,"
                                            + " but everything else works correctly."
                                            + " however, please inform the community about your configuration",
                                    t);
                }
                finally
                {
                    initializedListeners.add(getClass());
                }
            }
        }
    }

    public PhaseId getPhaseId()
    {
        return PhaseId.RESTORE_VIEW;
    }

    protected abstract void init();
}