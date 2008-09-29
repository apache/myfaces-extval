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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.core.InformationProviderBean;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.FactoryFinder;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseListener;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.lifecycle.LifecycleFactory;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;


/**
 * @author Gerhard Petracek
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValUtils
{
    public static String getBasePackage()
    {
        return getInformationProviderBean().getBasePackage();
    }

    public static InformationProviderBean getInformationProviderBean()
    {
        Map applicationMap = FacesContext.getCurrentInstance().getExternalContext().getApplicationMap();
        InformationProviderBean bean = (InformationProviderBean) applicationMap.get(InformationProviderBean.BEAN_NAME);

        if (bean == null)
        {
            return initInformationProviderBean(applicationMap);
        }
        return bean;
    }

    private static InformationProviderBean initInformationProviderBean(Map applicationMap)
    {
        List<String> informationProviderBeanClassNames = new ArrayList<String>();

        informationProviderBeanClassNames.add(WebXmlParameter.CUSTOM_CONVENTION_INFO_PROVIDER_BEAN);
        informationProviderBeanClassNames.add(ExtValUtils.getCustomInformationProviderBeanClassName());
        informationProviderBeanClassNames.add(InformationProviderBean.class.getName());

        InformationProviderBean informationProviderBean;
        for (String className : informationProviderBeanClassNames)
        {
            informationProviderBean = (InformationProviderBean) ClassUtils.tryToInstantiateClassForName(className);

            if (informationProviderBean != null)
            {
                applicationMap.put(InformationProviderBean.BEAN_NAME, informationProviderBean);
                return informationProviderBean;
            }
        }
        throw new IllegalStateException(InformationProviderBean.class.getName() + " not found");
    }

    public static String getCustomInformationProviderBeanClassName()
    {
        InformationProviderBean bean = (InformationProviderBean) ELUtils.getBean(InformationProviderBean.CUSTOM_BEAN);

        return (bean != null) ? bean.getClass().getName() : null;
    }

    public static void deregisterPhaseListener(PhaseListener phaseListener)
    {
        LifecycleFactory lifecycleFactory = (LifecycleFactory)FactoryFinder.getFactory(FactoryFinder.LIFECYCLE_FACTORY);

        String currentId;
        Lifecycle currentLifecycle;
        Iterator lifecycleIds = lifecycleFactory.getLifecycleIds();
        while (lifecycleIds.hasNext())
        {
            currentId = (String) lifecycleIds.next();
            currentLifecycle = lifecycleFactory.getLifecycle(currentId);
            currentLifecycle.removePhaseListener(phaseListener);
        }
    }
}
