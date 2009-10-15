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

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
class ExtValContextInternals
{
    boolean isComponentInitializationActivated()
    {
        return !"true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_COMPONENT_INITIALIZATION);
    }

    @SuppressWarnings({"unchecked"})
    InformationProviderBean initInformationProviderBean(Map applicationMap)
    {
        List<String> informationProviderBeanClassNames = new ArrayList<String>();

        informationProviderBeanClassNames.add(WebXmlParameter.CUSTOM_INFORMATION_PROVIDER_BEAN);
        informationProviderBeanClassNames.add(InformationProviderBean.CUSTOM_BEAN);

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

        tryToInitCustomConfiguredInformationProviderBeanClassName(applicationMap);

        if (applicationMap.containsKey(InformationProviderBean.BEAN_NAME))
        {
            return (InformationProviderBean) applicationMap.get(InformationProviderBean.BEAN_NAME);
        }
        return new InformationProviderBean();
    }

    @SuppressWarnings({"unchecked"})
    void tryToInitCustomConfiguredInformationProviderBeanClassName(Map applicationMap)
    {
        InformationProviderBean bean = (InformationProviderBean) ExtValUtils.getELHelper()
                .getBean(InformationProviderBean.CUSTOM_BEAN.replace(".", "_"));

        if (bean != null)
        {
            applicationMap.put(InformationProviderBean.BEAN_NAME, bean);
        }
    }

    InformationProviderBean getInformationProviderBean()
    {
        Map applicationMap = FacesContext.getCurrentInstance().getExternalContext().getApplicationMap();
        InformationProviderBean bean = (InformationProviderBean) applicationMap.get(InformationProviderBean.BEAN_NAME);

        if (bean == null)
        {
            return initInformationProviderBean(applicationMap);
        }
        return bean;
    }
}