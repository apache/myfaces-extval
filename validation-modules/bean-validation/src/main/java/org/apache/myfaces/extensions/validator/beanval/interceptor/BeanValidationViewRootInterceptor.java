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
package org.apache.myfaces.extensions.validator.beanval.interceptor;

import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;
import org.apache.myfaces.extensions.validator.beanval.group.SkipValidation;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.factory.ExtValAjaxBehavior;
import org.apache.myfaces.extensions.validator.core.interceptor.ViewRootInterceptor;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import javax.el.MethodExpression;
import javax.faces.component.ActionSource2;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.behavior.AjaxBehavior;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.AjaxBehaviorEvent;
import javax.faces.event.FacesEvent;
import java.lang.reflect.Method;
import java.util.List;

public class BeanValidationViewRootInterceptor implements ViewRootInterceptor
{
    public void afterQueueEvent(FacesEvent event)
    {
        UIComponent uiComponent = event.getComponent();
        if(event instanceof ActionEvent && uiComponent instanceof ActionSource2)
        {
            tryToProcessActionMethod((ActionSource2)event.getComponent());
        }
        if (event instanceof AjaxBehaviorEvent && uiComponent instanceof EditableValueHolder &&
                ((AjaxBehaviorEvent)event).getBehavior() instanceof AjaxBehavior)
        {
            tryToProcessAjaxListener(((AjaxBehavior)((AjaxBehaviorEvent)event).getBehavior()));
        }
    }

    private void tryToProcessActionMethod(ActionSource2 commandComponent)
    {
        MethodExpression actionExpression = commandComponent.getActionExpression();

        if(actionExpression == null)
        {
            return;
        }

        ELHelper elHelper = ExtValUtils.getELHelper();
        String actionString = actionExpression.getExpressionString();
        if(!elHelper.isELTermWellFormed(actionString))
        {
            return;
        }

        ValueBindingExpression valueBindingExpression = new ValueBindingExpression(actionString);

        FacesContext facesContext = FacesContext.getCurrentInstance();
        if (!ExtValUtils.getELHelper().isELTermValid(
                facesContext, valueBindingExpression.getBaseExpression().getExpressionString()))
        {
            return;
        }

        processBypassValidation(facesContext, valueBindingExpression, elHelper, ActionEvent.class);
    }

    private void tryToProcessAjaxListener(AjaxBehavior ajaxBehavior)
    {
        if (!(ajaxBehavior instanceof ExtValAjaxBehavior))
        {
            return;
        }

        List<MethodExpression> listenerExpressions = ((ExtValAjaxBehavior)ajaxBehavior).getListenerExpressions();

        ELHelper elHelper = ExtValUtils.getELHelper();

        for (MethodExpression listenerExpression : listenerExpressions)
        {
            String actionString = listenerExpression.getExpressionString();
            if(!elHelper.isELTermWellFormed(actionString))
            {
                continue;
            }

            ValueBindingExpression valueBindingExpression = new ValueBindingExpression(actionString);

            FacesContext facesContext = FacesContext.getCurrentInstance();
            if (!ExtValUtils.getELHelper().isELTermValid(
                    facesContext, valueBindingExpression.getBaseExpression().getExpressionString()))
            {
                continue;
            }

            processBypassValidation(facesContext, valueBindingExpression, elHelper, AjaxBehaviorEvent.class);
        }
    }

    private void processBypassValidation(FacesContext facesContext,
                                         ValueBindingExpression valueBindingExpression,
                                         ELHelper elHelper,
                                         Class... parameterTypes)
    {
        Object base = elHelper.getValueOfExpression(facesContext, valueBindingExpression.getBaseExpression());

        if (base == null)
        {
            return;
        }

        String methodName = valueBindingExpression.getProperty();

        if (methodName.contains("("))
        {
            //currently we just can support action-listeners and action-methods without parameters
            methodName = methodName.substring(0, methodName.indexOf("("));
        }

        Method actionMethod = ReflectionUtils.tryToGetMethod(ProxyUtils.getUnproxiedClass(base.getClass()), methodName);

        if (actionMethod == null)
        {
            actionMethod = ReflectionUtils.tryToGetMethod(ProxyUtils.getUnproxiedClass(base.getClass()),
                    methodName, parameterTypes);
        }

        if (actionMethod == null)
        {
            //TODO log unsupported expression
            return;
        }

        BeanValidation beanValidation = actionMethod.getAnnotation(BeanValidation.class);

        if(beanValidation == null)
        {
            return;
        }

        ExtValBeanValidationContext extValBeanValidationContext = ExtValBeanValidationContext.getCurrentInstance();

        String viewId = facesContext.getViewRoot().getViewId();

        //TODO log invalid expressions
        for (String condition : beanValidation.conditions())
        {
            if (elHelper.isELTermWellFormed(condition) &&
                    elHelper.isELTermValid(facesContext, condition))
            {
                if (Boolean.FALSE.equals(elHelper.getValueOfExpression(
                        facesContext, new ValueBindingExpression(condition))))
                {
                    return;
                }
            }
        }

        boolean skippedValidation = false;

        for (Class currentGroupClass : beanValidation.useGroups())
        {
            if(SkipValidation.class.isAssignableFrom(currentGroupClass))
            {
                skippedValidation = true;
                break;
            }
            extValBeanValidationContext.addGroup(currentGroupClass, viewId, null);
        }

        for (Class currentGroupClass : beanValidation.restrictGroups())
        {
            extValBeanValidationContext.restrictGroup(currentGroupClass, viewId, null);
        }

        if(skippedValidation)
        {
            extValBeanValidationContext.resetGroups(viewId);
            extValBeanValidationContext.addGroup(SkipValidation.class, viewId, null);
            extValBeanValidationContext.lockGroups(viewId);
        }
    }
}
