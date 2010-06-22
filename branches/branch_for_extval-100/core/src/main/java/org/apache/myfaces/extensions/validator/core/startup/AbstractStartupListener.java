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
import org.apache.myfaces.extensions.validator.util.WebXmlUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ProjectStageResolver;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;

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
    protected final Logger logger = Logger.getLogger(getClass().getName());

    //don't remove - it's a fallback if there is a problem with deregistration
    //target: don't process init logic more than once
    private static List<Class> initializedListeners = new ArrayList<Class>();

    private static boolean defaultProjectStageResolverInitialized = false;

    protected AbstractStartupListener()
    {
        logger.fine(getClass().getName() + " instantiated");
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
                    logger.info("start init of " + getClass().getName());

                    try
                    {
                        if(!isStartupListenerDeactivated())
                        {
                            initModuleConfig();

                            initProjectStageResolver();

                            init();
                        }
                        else
                        {
                            logger.info("init of " + getClass().getName() + " deactivated");
                        }

                        logger.info("init of " + getClass().getName() + " finished");
                    }
                    finally
                    {
                        JsfUtils.deregisterPhaseListener(this);
                    }
                }
                catch (Exception e)
                {
                        logger.log(Level.WARNING,
                                "an exception occurred while deregistering the phase-listener"
                                + getClass().getName()
                                + " -> there is just a little overhead,"
                                + " but everything else works correctly."
                                + " however, please inform the community about your configuration", e);
                }
                finally
                {
                    initializedListeners.add(getClass());
                }
            }
        }
    }

    protected void initModuleConfig()
    {
        //override if needed
    }

    public PhaseId getPhaseId()
    {
        return PhaseId.RESTORE_VIEW;
    }

    protected boolean isStartupListenerDeactivated()
    {
        return ExtValUtils.isExtValDeactivated() ||
                "true".equalsIgnoreCase(WebXmlUtils.getInitParameter(null, getClass().getName() + ":DEACTIVATED"));
    }

    protected void initProjectStageResolver()
    {
        if(!defaultProjectStageResolverInitialized)
        {
            ExtValContext.getContext()
                    .addGlobalProperty(ProjectStageResolver.class.getName(), getProjectStageResolver(), false);
            defaultProjectStageResolverInitialized = true;
        }
    }

    protected ProjectStageResolver getProjectStageResolver()
    {
        return ExtValCoreConfiguration.get().projectStageResolver();
    }

    protected abstract void init();
}
