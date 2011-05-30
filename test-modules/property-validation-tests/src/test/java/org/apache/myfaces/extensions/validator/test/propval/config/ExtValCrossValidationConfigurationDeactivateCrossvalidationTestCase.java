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
package org.apache.myfaces.extensions.validator.test.propval.config;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.faces.FactoryFinder;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.lifecycle.LifecycleFactory;
import javax.faces.webapp.FacesServlet;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.crossval.DefaultExtValCrossValidationModuleConfiguration;
import org.apache.myfaces.extensions.validator.crossval.ExtValCrossValidationModuleConfiguration;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCrossValidationConfigurationDeactivateCrossvalidationTestCase extends
        ExtValCrossValidationConfigurationTestCase
{

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_CROSSVALIDATION", "true");

        }
    }

    @Override
    protected ExtValCrossValidationModuleConfiguration getCustomCrossValidationModuleConfiguration()
    {
        return new DefaultExtValCrossValidationModuleConfiguration()
        {

            @Override
            public boolean deactivateCrossvalidation()
            {
                return true;
            }

        };
    }

    @Test
    public void testExtValBaseValidationConfigurationDeactivateCrossvalidationDefault()
    {
        executeBeforePhaseOfCrossValidationPhaseListener();
        // PhaseIdPhaseListener from core and CrossValidationPhaseListener
        Assert.assertEquals(2, getPhaseListeners().size());
    }

    @Test
    public void testExtValBaseValidationConfigurationDeactivateCrossvalidationWebXml()
    {
        executeBeforePhaseOfCrossValidationPhaseListener();

        // PhaseIdPhaseListener from core
        Assert.assertEquals(1, getPhaseListeners().size());
    }

    @Test
    public void testExtValBaseValidationConfigurationDeactivateCrossvalidationCustomConfig()
    {
        executeBeforePhaseOfCrossValidationPhaseListener();

        // PhaseIdPhaseListener from core
        Assert.assertEquals(1, getPhaseListeners().size());

    }

    private void executeBeforePhaseOfCrossValidationPhaseListener()
    {
        for (PhaseListener listener : getPhaseListeners())
        {
            listener.beforePhase(new PhaseEvent(facesContext, PhaseId.RESTORE_VIEW, getLifeCycle()));
        }
    }

    private Lifecycle getLifeCycle()
    {
        LifecycleFactory factory = (LifecycleFactory) FactoryFinder.getFactory(FactoryFinder.LIFECYCLE_FACTORY);
        String id = facesContext.getExternalContext().getInitParameter(FacesServlet.LIFECYCLE_ID_ATTR);
        if (id == null)
        {
            id = LifecycleFactory.DEFAULT_LIFECYCLE;
        }
        return factory.getLifecycle(id);
    }

    private List<PhaseListener> getPhaseListeners()
    {
        LifecycleFactory lifecycleFactory = (LifecycleFactory) FactoryFinder
                .getFactory(FactoryFinder.LIFECYCLE_FACTORY);

        String currentId;
        Lifecycle currentLifecycle;
        Iterator lifecycleIds = lifecycleFactory.getLifecycleIds();
        assert lifecycleIds.hasNext();
        currentId = (String) lifecycleIds.next();
        currentLifecycle = lifecycleFactory.getLifecycle(currentId);
        return Arrays.asList(currentLifecycle.getPhaseListeners());

    }

}
