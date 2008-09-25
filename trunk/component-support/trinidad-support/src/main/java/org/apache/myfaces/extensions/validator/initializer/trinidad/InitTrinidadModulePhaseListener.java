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
package org.apache.myfaces.extensions.validator.initializer.trinidad;

import org.apache.myfaces.extensions.validator.core.AbstractStartupConfigListener;
import org.apache.myfaces.extensions.validator.core.ExtValValidationPhaseListener;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.initializer.trinidad.component.TrinidadComponentInitializer;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;

import javax.faces.lifecycle.LifecycleFactory;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.event.PhaseListener;
import javax.faces.FactoryFinder;
import java.util.Iterator;

/**
 * @author Gerhard Petracek
 */
public class InitTrinidadModulePhaseListener extends AbstractStartupConfigListener
{
    @ToDo(value = Priority.MEDIUM, description = "web.xml parameter to deactivate it")
    protected void init()
    {
        deregisterExtValValidationPhaseListener();
        initTrinidadSupport();
    }

    private void deregisterExtValValidationPhaseListener()
    {
        LifecycleFactory lifecycleFactory = (LifecycleFactory) FactoryFinder
            .getFactory(FactoryFinder.LIFECYCLE_FACTORY);

        String currentId;
        Lifecycle currentLifecycle;
        Iterator lifecycleIds = lifecycleFactory.getLifecycleIds();

        while (lifecycleIds.hasNext())
        {
            currentId = (String) lifecycleIds.next();
            currentLifecycle = lifecycleFactory.getLifecycle(currentId);
            for(PhaseListener currentPhaseListener : currentLifecycle.getPhaseListeners())
            {
                if(currentPhaseListener instanceof ExtValValidationPhaseListener)
                {
                    currentLifecycle.removePhaseListener(currentPhaseListener);
                }
            }
        }
    }

    private void initTrinidadSupport()
    {
        String deactivateClientSideValidation = WebXmlParameter.DEACTIVATE_CLIENT_SIDE_TRINIDAD_VALIDATION;

        if(deactivateClientSideValidation == null || !deactivateClientSideValidation.equalsIgnoreCase("true"))
        {
            ExtValContext.getContext().addComponentInitializer(new TrinidadComponentInitializer());
        }
    }
}