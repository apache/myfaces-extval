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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.event.PhaseListener;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.lifecycle.LifecycleFactory;

import org.apache.myfaces.extensions.validator.core.InformationProviderBean;
import org.apache.myfaces.extensions.validator.core.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;


/**
 * @author Gerhard Petracek
 */
public class ExtValUtils
{

    public static String getBasePackage()
    {
        return getInformationProviderBean().getBasePackage();
    }

    public static InformationProviderBean getInformationProviderBean()
    {
        Map applicationMap = FacesContext.getCurrentInstance()
                .getExternalContext().getApplicationMap();
        InformationProviderBean bean = (InformationProviderBean) applicationMap
                .get(InformationProviderBean.BEAN_NAME);

        if (bean == null)
        {
            return initInformationProviderBean(applicationMap);
        }
        return bean;
    }

    private static InformationProviderBean initInformationProviderBean(
            Map applicationMap)
    {

        List<String> informationProviderBeanClassNames = new ArrayList<String>();

        informationProviderBeanClassNames
                .add(WebXmlParameter.CUSTOM_CONVENTION_INFO_PROVIDER_BEAN);
        informationProviderBeanClassNames.add(ExtValUtils
                .getCustomInformationProviderBeanClassName());
        informationProviderBeanClassNames.add(InformationProviderBean.class
                .getName());

        InformationProviderBean informationProviderBean;
        for (String className : informationProviderBeanClassNames)
        {
            informationProviderBean = (InformationProviderBean) ClassUtils
                    .tryToInstantiateClassForName(className);

            if (informationProviderBean != null)
            {
                applicationMap.put(InformationProviderBean.BEAN_NAME,
                        informationProviderBean);
                return informationProviderBean;
            }
        }
        throw new IllegalStateException(InformationProviderBean.class.getName()
                + " not found");
    }

    public static String getCustomInformationProviderBeanClassName()
    {
        InformationProviderBean bean = (InformationProviderBean) ELUtils
                .getBean(InformationProviderBean.CUSTOM_BEAN);

        return (bean != null) ? bean.getClass().getName() : null;
    }

    public static void deregisterPhaseListener(PhaseListener phaseListener)
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
            currentLifecycle.removePhaseListener(phaseListener);
        }
    }

    public static final String VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY = ExtValUtils.class
            .getName();

    public static Map<String, ProcessedInformationEntry> getOrInitValueBindingConvertedValueMapping()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext()
                .getRequestMap();

        if (!requestMap.containsKey(VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY))
        {
            resetCrossValidationStorage();
        }

        return (Map<String, ProcessedInformationEntry>) requestMap
                .get(VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY);
    }

    public static void resetCrossValidationStorage()
    {
        FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
                .put(VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY,
                        new HashMap<String, ProcessedInformationEntry>());
    }

    /*
     * workaround: mapping clientId -> proxy -> after restore view: find component + set converter of the mapping
     * TODO: find a better solution - multi-window-mode
     */
    public static final String PROXY_MAPPING_KEY = VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY
            + ":proxyMapping";

    public static Map<String, Object> getOrInitProxyMapping()
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        //session scope is just the worst case - cleanup after restore view
        Map sessionMap = facesContext.getExternalContext().getSessionMap();

        String viewId = facesContext.getViewRoot().getViewId();

        if (!sessionMap.containsKey(PROXY_MAPPING_KEY)
                || !((Map) sessionMap.get(PROXY_MAPPING_KEY))
                        .containsKey(viewId))
        {
            resetProxyMapping(viewId);
        }

        return (Map<String, Object>) ((Map) sessionMap.get(PROXY_MAPPING_KEY))
                .get(viewId);
    }

    public static void resetProxyMapping(String viewId)
    {
        Map sessionMap = FacesContext.getCurrentInstance().getExternalContext()
                .getSessionMap();

        Map<String, Map<String, Object>> storage;

        if (sessionMap.containsKey(PROXY_MAPPING_KEY))
        {
            storage = (Map) sessionMap.get(PROXY_MAPPING_KEY);
        }
        else
        {
            storage = new HashMap<String, Map<String, Object>>();
        }

        Map<String, Object> map;
        if (!storage.containsKey(viewId))
        {
            map = new HashMap<String, Object>();
            storage.put(viewId, map);
        }

        sessionMap.put(PROXY_MAPPING_KEY, storage);
    }

    public static final String PROCESSED_CONVERTER_COUNT_KEY = VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY
            + ":processedConverterCount";

    public static Integer getProcessedConverterCount()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext()
                .getRequestMap();

        if (!requestMap.containsKey(PROCESSED_CONVERTER_COUNT_KEY))
        {
            resetProcessedConverterMapping();
        }

        return (Integer) requestMap.get(PROCESSED_CONVERTER_COUNT_KEY);
    }

    public static void setProcessedConverterCount(Integer count)
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext()
                .getRequestMap();

        if (!requestMap.containsKey(PROCESSED_CONVERTER_COUNT_KEY))
        {
            resetProcessedConverterMapping();
        }

        requestMap.put(PROCESSED_CONVERTER_COUNT_KEY, count);
    }

    public static void resetProcessedConverterMapping()
    {
        FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
                .put(PROCESSED_CONVERTER_COUNT_KEY, 0);
    }

    public static void increaseProcessedConverterCount()
    {
        setProcessedConverterCount(getProcessedConverterCount() + 1);
    }

    public static void decreaseProcessedConverterCount()
    {
        setProcessedConverterCount(getProcessedConverterCount() - 1);
    }

    public static boolean useProxyMapping()
    {

        String initParam = WebXmlParameter.DEACTIVATE_PROXY_MAPPING;
        boolean disableProxyMapping = (initParam != null && initParam.trim()
                .equalsIgnoreCase("true"));

        return !(useFallbackAdapters() || disableProxyMapping);
    }

    @Deprecated
    public static boolean useFallbackAdapters()
    {
        String initParam = WebXmlParameter.USE_ADAPTERS;
        return (initParam != null && initParam.trim().equalsIgnoreCase("true"));
    }

    public static void restoreProxies()
    {
        UIViewRoot viewRoot = FacesContext.getCurrentInstance().getViewRoot();

        if (viewRoot != null && ExtValUtils.useProxyMapping())
        {
            Map componentConverterMapping = ExtValUtils.getOrInitProxyMapping();

            Iterator current = componentConverterMapping.keySet().iterator();
            String key;
            Converter converter;
            Converter converterOfComponent;
            UIComponent component = null;
            while (current.hasNext())
            {
                key = (String) current.next();
                converter = (Converter) componentConverterMapping.get(key);

                try
                {
                    component = viewRoot.findComponent(key);
                }
                catch (IllegalArgumentException e)
                {
                    //do nothing - it's just a ri bug with complex components - 
                    //resolveComponentInComplexComponent will return the correct component
                }

                if (component == null)
                {
                    component = resolveComponentInComplexComponent(viewRoot,
                            component, key);

                    if (component == null)
                    {
                        continue;
                    }
                }

                if (!(component instanceof ValueHolder))
                {
                    continue;
                }

                converterOfComponent = ((ValueHolder) component).getConverter();

                //converterOfComponent lost callback during state-saving -> set converter of same type
                if (converterOfComponent != null
                        && converterOfComponent.getClass().getSuperclass()
                                .equals(converter.getClass().getSuperclass()))
                {
                    ((ValueHolder) component).setConverter(converter);
                }
            }
        }

        if (ExtValUtils.useProxyMapping())
        {
            ExtValUtils.resetProxyMapping(FacesContext.getCurrentInstance()
                    .getViewRoot().getViewId());
        }
    }

    //TODO
    private static UIComponent resolveComponentInComplexComponent(
            UIComponent viewRoot, UIComponent component, String key)
    {
        int index = key.lastIndexOf(":");

        if (index == -1)
        {
            return null;
        }

        String newKey = key.substring(0, index);
        if (viewRoot.findComponent(newKey) == null)
        {
            int newIndex = newKey.lastIndexOf(":");
            if (newIndex < 1)
            {
                return null;
            }
            newKey = newKey.substring(0, newIndex);

            component = viewRoot.findComponent(newKey);

            if (component == null)
            {
                return null;
            }
            else
            {
                return tryToResolveChildComponent(component, key.substring(key
                        .lastIndexOf(":")));
            }
        }
        return null;
    }

    //TODO
    private static UIComponent tryToResolveChildComponent(
            UIComponent component, String endOfKey)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        String clientId = component.getClientId(facesContext);

        if (clientId.contains(":")
                && clientId.substring(clientId.lastIndexOf(":")).endsWith(
                        endOfKey))
        {
            return component;
        }

        UIComponent foundComponent;
        for (UIComponent child : (List<UIComponent>) component.getChildren())
        {
            foundComponent = tryToResolveChildComponent(child, endOfKey);

            if (foundComponent != null)
            {
                return foundComponent;
            }
        }

        return null;
    }

    public static final String ORIGINAL_APPLICATION_KEY = VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY
            + ":wrapped_application";

    //in order to access the wrapped application and support other Application wrappers
    public static void setOriginalApplication(Application application)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        Map applicationMap = facesContext.getExternalContext()
                .getApplicationMap();

        if (!applicationMap.containsKey(ORIGINAL_APPLICATION_KEY))
        {
            synchronized (ExtValUtils.class)
            {
                applicationMap.put(ORIGINAL_APPLICATION_KEY, application);
            }
        }
    }

    public static Application getOriginalApplication()
    {
        return (Application) FacesContext.getCurrentInstance()
                .getExternalContext().getApplicationMap().get(
                        ORIGINAL_APPLICATION_KEY);
    }

    public static Converter tryToCreateOriginalConverter(
            FacesContext facesContext, UIComponent uiComponent)
    {
        //for backward compatibility: cross-validation workaround with hidden field and static value
        Class valueBindingType = ELUtils.getTypeOfValueBindingForComponent(
                facesContext, uiComponent);

        if (valueBindingType == null)
        {
            return null;
        }

        return getOriginalApplication().createConverter(valueBindingType);
    }
}
