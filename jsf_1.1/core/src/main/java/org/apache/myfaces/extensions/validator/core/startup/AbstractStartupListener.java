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
package org.apache.myfaces.extensions.validator.core.startup;

import org.apache.myfaces.extensions.validator.util.JsfUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import java.util.ArrayList;
import java.util.List;

/**
 * In order to execute logic just once.
 * e.g. register artifacts via api
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractStartupListener implements PhaseListener
{
    protected final Log logger = LogFactory.getLog(getClass());

    //don't remove - it's a fallback if there is a problem with deregistration
    //target: don't process init logic more than once
    private static List<Class> initializedListeners = new ArrayList<Class>();

    protected AbstractStartupListener()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public void afterPhase(PhaseEvent event)
    {
    }

    public void beforePhase(PhaseEvent event)
    {
        synchronized (AbstractStartupListener.class)
        {
            if (!initializedListeners.contains(getClass()))
            {
                try
                {
                    if(logger.isInfoEnabled())
                    {
                        logger.info("start init of " + getClass().getName());
                    }

                    try
                    {
                        init();

                        if(logger.isInfoEnabled())
                        {
                            logger.info("init of " + getClass().getName() + " finished");
                        }
                    }
                    finally
                    {
                        JsfUtils.deregisterPhaseListener(this);
                    }
                }
                catch (Throwable t)
                {
                    if(logger.isWarnEnabled())
                    {
                        logger.warn("an exception occurred while deregistering the phase-listener"
                                + getClass().getName()
                                + " -> there is just a little overhead,"
                                + " but everything else works correctly."
                                + " however, please inform the community about your configuration", t);
                    }
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
