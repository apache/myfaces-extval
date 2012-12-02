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
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
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
 * Startup listeners can be used to execute e.g. setup-logic just once before the first request gets processed.
 * After a listener was executed it gets deactivated.
 *
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

    /**
     * {@inheritDoc}
     * 
     * Is responsible for executing {@link #init()}. Before the method is executed, the start-up
     * listener has the chance of putting a configuration object in place (@see #initModuleConfig).
     * Startup listeners can be deactivated via a web.xml context-param.
     */
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

    /**
     * Allows subclasses to put some configuration in place, before the actual initialization code is performed.
     * @see org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration#use(org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration, boolean)
     */
    protected void initModuleConfig()
    {
        //override if needed
    }

    /**
     * {@inheritDoc}
     */
    public PhaseId getPhaseId()
    {
        return PhaseId.RESTORE_VIEW;
    }

    /**
     * Startup listeners can be deactivated via context-params in the web.xml.<br/>
     * Example:
     * <context-param>
     *   <param-name>fully.qualified.StartupListener:DEACTIVATED</param-name>
     *   <param-value>true</param-value>
     * </context-param>
     *
     * @return true if the current instance is deactivated - false otherwise
     */
    protected boolean isStartupListenerDeactivated()
    {
        return ExtValUtils.isExtValDeactivated() ||
                "true".equalsIgnoreCase(WebXmlUtils.getInitParameter(null, getClass().getName() + ":DEACTIVATED"));
    }

    @ToDo(value=Priority.LOW, description="ProjectStage#getCurrentProjectStage is using the config and not the global" +
            "property. align.")
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

    /**
     * Contains logic which should get executed before the application gets invoked
     * (e.g. initialization code of a module or add-on).
     */
    protected abstract void init();
}
